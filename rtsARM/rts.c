/*
 * Copyright (c) 2009, Per Lindgren, Johan Eriksson, Johan Nordlander,
 * Simon Aittamaa.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the Luleå University of Technology nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "rts.h"

#include <lpc2468_registers.h>

// Context switching ------------------------------------------------------------------------------

#define ARM7_CONTEXT_SAVE() \
	__asm__ __volatile__ (\
		/* r0 & r1 is used as a temporary register, we need lr in swi mode. */\
		"stmfd	sp!, {r0,r1,lr}\n"\
		/* Disable interrupts */\
		"mrs	r0, cpsr\n"\
		"orr	r0, r0, #0xc0\n"\
		"msr	cpsr_c, r0\n"\
		/* Get the user mode stack pointer value. */\
		"stmdb	sp, {sp}^\n"\
		"nop\n"\
		"ldmdb	sp, {r0}\n"\
		/* Store the return address at the first/highest address. */\
		"sub	lr, lr, #4\n"\
		"stmfd	r0!, {lr}\n"\
		/* Start using lr and restore old r0. */\
		"mov	lr, r0\n"\
		"ldmfd	sp, {r0}\n"\
		/* Save all user mode registers. */\
		"stmfd	lr, {r0-r14}^\n"\
		"nop\n"\
		"sub	lr, lr, #60\n"\
		/* Save the saved process register. */\
		"mrs	r0, SPSR\n"\
		"stmfd	lr!, {r0}\n"\
		/* Save the stack pointer to the the current_thread variable. */\
		"ldr	r0, =current_thread\n"\
		"ldr	r0, [r0]\n"\
		"str	lr, [r0]\n"\
		/* Check the context cookie. */\
		"ldr	r0, [r0, #4]\n"\
		"ldr	r0, [r0]\n"\
		"ldr	r1, =context_cookie\n"\
		"ldr	r1, [r1]\n"\
		"cmp	r0, r1\n"\
		"bne	context_panic\n"\
		/* \
		 * Restore the old r0 _again_, this is so we can use this macro\
		 * in the software interrupt as well. Don't forget the original\
		 * lr as well for interrupt id.\
		 */\
		"ldmfd	sp!, {r0,r1,lr}\n"\
		)

#define ARM7_CONTEXT_RESTORE() \
	__asm__ __volatile__ (\
		/* Load the current context stack pointer. */\
		"ldr	r0, =current_thread\n"\
		"ldr	r0, [r0]\n"\
		"ldr	lr, [r0]\n"\
		/* Check the context cookie. */\
		"ldr	r0, [r0, #4]\n"\
		"ldr	r0, [r0]\n"\
		"ldr	r1, =context_cookie\n"\
		"ldr	r1, [r1]\n"\
		"cmp	r0, r1\n"\
		"bne	context_panic\n"\
		/* Restore the saved saved process status. */\
		"ldmfd	lr!, {r0}\n"\
		"msr	SPSR, r0\n"\
		/* Restore the user context. */\
		"ldmfd	lr, {r0-r14}^\n"\
		"nop\n"\
		"add	lr, lr, #60\n"\
		/* Get the return address and return(leaves interrupt..). */\
		"ldmfd	lr, {pc}^\n"\
        )

__attribute__((naked)) void dispatch( Thread next ) {   
    // Note: parameter 'next' is vital -- goes into r0
    // See swi_handler() below.
	asm volatile(
		"swi 0\n"
		"mov pc, lr\n"
	);
}


// Macros ----------------------------------------------------------------------------------------

#define NTHREADS        5
#define STACKSIZE       WORDS(0x10000)  // 0x10000 bytes = 64 kB

#define SLEEP()         /* not yet defined */

#define TDELTA          1
#define TIMERINIT()     { T0TCR = 0; T0TCR = 1; /* stop & reset clock */ \
	                      T0IR = (1<<0);        /* MR0 int en */ \
	                      T0PR = (576 -1);      /* 1tick = 10uS , at MCLK = 57.6Mhz*/ \
	                      T0MCR = (1<<0); \
	                      VICVectAddr4 = (unsigned long)timer0_interrupt; \
	                      VICIntEnable |= (1<<4); \
                        }
#define TIMERGET(x)     { x.tv_usec = T0TC; x.tv_sec = x.tv_usec / 100000; x.tv_usec = (x.tv_usec % 100000) * 10; }
#define TIMERSET(x,now) { T0MR0 = (x).tv_sec*100000 + (x).tv_usec / 10; \
                          if ((T0MR0 < T0TC)) \
                              VICSoftInt = 1<<4; \
                        }
#define TIMERACK()      { VICSoftIntClr = (1<<4); T0IR = T0IR; }


// Globals --------------------------------------------------------------------------------------

struct Msg msg0         = { NULL, NULL, NULL, NULL, NULL, {0, 0}, {0, 0} };

struct Thread threads[NTHREADS];

// the idle (=main) thread
struct Thread threadI;

Msg savedMsg            = NULL;
Msg msgQ                = NULL;
Msg timerQ              = NULL;

Thread threadPool       = &threads[0];
Thread activeStack      = &threadI;
Thread current_thread   = &threadI;

extern WORD _end;
extern WORD __ram_end;

WORD context_cookie     = 0x55aa55aa;

WORD thestacks[NTHREADS*STACKSIZE];


// Printing & halting ------------------------------------------------------------------------------

void debug_char(char c) {
		while (!(U0LSR & (1<<5)));
		U0THR = c;    
		if (c == '\n') {
            while (!(U0LSR & (1<<5)));
            U0THR = '\r';
		}
}

void debug(char *msg) {
	while (*msg)
        debug_char(*msg++);
}

void debug_hex(unsigned long value) {
	static char hex[] = "0123456789abcdef";

	while (!(U0LSR & (1<<5)));

	U0THR = hex[value>>28&0xf];
	while (!(U0LSR & (1<<5)));

	U0THR = hex[value>>24&0xf];
	while (!(U0LSR & (1<<5)));

	U0THR = hex[value>>20&0xf];
	while (!(U0LSR & (1<<5)));

	U0THR = hex[value>>16&0xf];
	while (!(U0LSR & (1<<5)));

	U0THR = hex[value>>12&0xf];
	while (!(U0LSR & (1<<5)));

	U0THR = hex[value>>8&0xf];
	while (!(U0LSR & (1<<5)));

	U0THR = hex[value>>4&0xf];
	while (!(U0LSR & (1<<5)));

	U0THR = hex[value&0xf];
	while (!(U0LSR & (1<<5)));
}

void panic(char *str) {
	PROTECT(1);
	debug(str);
	for (;;);
}

void context_panic(void) {
	panic("Context cookie corrupted.\r\n");
}


// Memory management --------------------------------------------------------------------------------

#include "gc.c"


// Cyclic data handling -----------------------------------------------------------------------------

#include "cyclic.c"


// GCINFO definitions for the built-in types -----------------------------------------------------

#include "timber.c"


// Queue management ------------------------------------------------------------------------------

void enqueueMsgQ(Msg p) {
        Msg prev = NULL, q = msgQ;
        while (q && ABS_LE(q->deadline, p->deadline)) {
                prev = q;
                q = q->next;
        }
        p->next = q;
        if (prev == NULL)
                msgQ = p;
        else
                prev->next = p;
}

void enqueueTimerQ(Msg p) {
        Msg prev = NULL, q = timerQ;
        while (q && ABS_LE(q->baseline, p->baseline)) {
                prev = q;
                q = q->next;
        }
        p->next = q;
        if (prev == NULL)
                timerQ = p;
        else
                prev->next = p;
}

void activate(void) {
    Thread t = threadPool;
    threadPool = threadPool->next;
    t->next = activeStack;
    activeStack = t;
}

void deactivate(void) {
    Thread t = activeStack;
    activeStack = activeStack->next;
    t->next = threadPool;
    threadPool = t;
}


// Major primitives ---------------------------------------------------------------------

void IRQ_PROLOGUE(void) {
    savedMsg = current_thread->msg;
    current_thread->msg = &msg0;
    TIMERGET(msg0.baseline);
}

void IRQ_EPILOGUE(void) {
    current_thread->msg = savedMsg;
    Msg topMsg = activeStack->msg;
    if (msgQ && threadPool && ((!topMsg) || ABS_LT(msgQ->deadline, topMsg->deadline))) {
        activate();
		current_thread = activeStack;
    }
}

void RUN(void) {
    while (1) {
        Msg this = current_thread->msg = msgQ;
        msgQ = msgQ->next;
        PROTECT(0);

        this->Obj = LOCK(this->Obj);
        UNIT (*code)(Msg,OID) = this->Code;
        if (code)
            code(this, this->Obj);
        UNLOCK(this->Obj);
            
        PROTECT(1);
        current_thread->msg = NULL;
        Msg oldMsg = activeStack->next->msg;
                
        if (!msgQ || (oldMsg && ABS_LT(oldMsg->deadline, msgQ->deadline))) {
            deactivate();
            Thread t = activeStack;                     // can't be NULL, may be &threadI
            while (t->waitsFor) 
                t = ((Ref)t->waitsFor)->ownedBy;
            dispatch(t);
        }
    } 
}

Msg ASYNC( Msg m, Time bl, Time dl ) {
    if (bl) {
        m->baseline = current_thread->msg->baseline;
        ABS_ADD(m->baseline, bl);

        if (dl) {
	        m->deadline = m->baseline;
            ABS_ADD(m->deadline, dl);
	    } else if (ABS_LT(m->baseline, current_thread->msg->deadline))
	        m->deadline = current_thread->msg->deadline;
	    else
            m->deadline = absInfinity;

        int status = ISPROTECTED();
        PROTECT(1);
    
        m->sender = NULL;
        AbsTime now;
        TIMERGET(now);
        if (ABS_LT(now, m->baseline)) {
	        enqueueTimerQ(m);
	        rootsDirty = 1;
	        if (timerQ == m)
		        TIMERSET(timerQ->baseline, now);
        } else
            enqueueMsgQ(m);
            
        PROTECT(status);
    } else {
        m->Obj = LOCK(m->Obj);
        UNIT (*code)(Msg,OID) = m->Code;
        if (code)
            code(m, m->Obj);
        UNLOCK(m->Obj);
    }
    return m;
}

void INITREF( Ref obj ) {
	obj->GCINFO = __GC__Ref;
	obj->wantedBy = 0;
	obj->ownedBy = 0;
    obj->STATE = (ADDR)STATEOF(obj);                        // actually unused, but keep it clean
}

OID LOCK( OID obj ) {
	int status = ISPROTECTED();
    PROTECT(1);
    
    GC_PROLOGUE(obj);
    Thread t = obj->ownedBy;
    if (t) {                                                // "obj" is already locked
        while (t->waitsFor)
            t = t->waitsFor->ownedBy;
        if (t == current_thread)                            // deadlock
            panic("Deadlock");
        if (obj->wantedBy)
            obj->wantedBy->waitsFor = NULL;
        obj->wantedBy = current_thread;
        current_thread->waitsFor = obj;
        dispatch(t);
	    GC_PROLOGUE(obj);									// in case t was the GC
    }
    obj->ownedBy = current_thread;

    PROTECT(status);
    return obj;
}

void GC_LOCK( OID obj ) {
    PROTECT(1);
    Thread t = obj->ownedBy;
    if (t)
        panic("GC found locked object");
    obj->ownedBy = current_thread;
    PROTECT(0);
}

UNIT UNLOCK( OID obj ) {
    int status = ISPROTECTED();
    PROTECT(1);

    obj->ownedBy = NULL;
    Thread t = obj->wantedBy;
    if (t) {                                                // we have run on someone's behalf
        obj->wantedBy = NULL;
        t->waitsFor = NULL;
        dispatch(t);
    }
	GC_EPILOGUE(obj);

    PROTECT(status);
    return (UNIT)0;
}

void GC_UNLOCK( OID obj ) {
    PROTECT(1);
    obj->ownedBy = NULL;
    Thread t = obj->wantedBy;
    if (t) {                                                // we have run on someone's behalf
        obj->wantedBy = NULL;
        t->waitsFor = NULL;
        dispatch(t);
    }
    PROTECT(0);
}

UNIT ABORT(BITS32 polytag, Msg m, Ref dummy) {
    m->Code = NULL;
    ADDR info;
    do {
        info = IND0((ADDR)m);
        if (ISODD(info))
            ((Msg)((ADDR)m)[1])->Code = NULL;
    } while (info != IND0((ADDR)m));
    return (UNIT)0;
}

void RAISE(Int err) {
    panic("Unhandled exception");
}

POLY Raise(BITS32 polyTag, Int err) {
    RAISE(err);
    return NULL;
}


// Arrays ---------------------------------------------------------------------------------------------

#include "arrays.c"


// Timer ----------------------------------------------------------------------------------------------

#include "timer.c"


// Show Float -----------------------------------------------------------------------------------------

int snprintf(char *s, int n, const char *format, ...) {
    char *p = "<float>";
    while (*p)
        *s++ = *p++;
    return 7;
}
     
#include "float.c"


// The timer queue -------------------------------------------------------------------------------------

void scanTimerQ(void) {
	PROTECT(1);
	if (timerQ) {
		timerQ = (Msg)copy((ADDR)timerQ);
        Msg m = timerQ;
	    PROTECT(0);
	    PROTECT(1);
		Msg next = m->next;
		while (next) {
			m->next = (Msg)copy((ADDR)next);
			m = m->next;
	        PROTECT(0);
	        PROTECT(1);
			next = m->next;
		}
	}
	PROTECT(0);
}

struct Scanner timerQscanner = { scanTimerQ, NULL };

void timer0_interrupt(void) {
    IRQ_PROLOGUE();
    TIMERACK();
    while (timerQ && ABS_LE(timerQ->baseline, msg0.baseline)) {
        Msg m = timerQ;
        timerQ = timerQ->next;
        enqueueMsgQ(m);
    }
    if (timerQ)
        TIMERSET(timerQ->baseline, msg0.baseline);
    IRQ_EPILOGUE();
}


// Bare metal exception handlers ----------------------------------------------------------------------

void *bad_pc = 0;

__attribute__((naked)) void undef_handler(void) {
    asm volatile(
		"ldr	r0, =bad_pc\n"
		"str	lr, [r0]\n"
	);
    panic("Undefined instruction exception\r\n");
}

__attribute__((naked)) void code_abort_handler(void) {
    asm volatile(
		"ldr	r0, =bad_pc\n"
		"str	lr, [r0]\n"
	);
    panic("Code abort exception\r\n");
}

__attribute__((naked)) void data_abort_handler(void) {
    asm volatile(
		"ldr	r0, =bad_pc\n"
		"str	lr, [r0]\n"
	);
    panic("Data abort exception\r\n");
}

__attribute__((naked)) void fiq_handler(void) {
    asm volatile(
		"ldr	r0, =bad_pc\n"
		"str	lr, [r0]\n"
	);
    panic("FIQ exception\r\n");
}

void *vic_ivr = (void*)&VICVectAddr;

__attribute__((naked))  void irq_handler(void) {
	ARM7_CONTEXT_SAVE();
	asm volatile(
		"ldr	r0, =vic_ivr\n"
		"ldr	r0, [r0]\n"
		"ldr	r0, [r0]\n"
		"mov	lr, pc\n"
		"bx		r0\n"

		"ldr	r0, =vic_ivr\n"
		"ldr	r0, [r0]\n"
		"str	r0, [r0]\n"
	);
	ARM7_CONTEXT_RESTORE();
}

__attribute__((naked)) void swi_handler(void) {
	// Adjudt the link register so that we can treat this as a regular
	// interrupt ie. use save/restore context macros.
	// Note: expects Thread parameter in r0
	asm volatile("add	lr, lr, #4\n");
	ARM7_CONTEXT_SAVE();
	asm volatile(
	    "ldr    r4, =current_thread\n"
	    "str    r0, [r4]\n"
	);
	ARM7_CONTEXT_RESTORE();
}


// Startup -----------------------------------------------------------------------------------------

void idle(void) {
    while (1) {
        PROTECT(1);
        if (heapLevel(16) > 13)
		    gc(0);
        PROTECT(0);
        SLEEP();
	}
}

void startup(Time_Time_to_Msg prog) {
    prog->Code(prog, Inherit, Inherit);
    current_thread->msg = NULL;
    if (msgQ) {
        activate();
        dispatch(activeStack);
    }
    idle();
}


// Initialization -------------------------------------------------------------------------------------

void thread_init(int i) {
    int j;
    arm7_context_t *context = &threads[i].context;
    
    context->cookie = &thestacks[i*STACKSIZE];
	*context->cookie = context_cookie;
    context->sp = &thestacks[(i+1)*STACKSIZE];

	ADDR sp = context->sp;

	// Push the return address
	*(--context->sp) = (WORD)RUN;

	// Push a bogus link register
	*(--context->sp) = 0x00000000;

	// Push the stack pointer
	*(--context->sp) = (WORD)sp;

	// Push some fake registers
	for (j=13; j>0; j--)
		*(--context->sp) = 0x00000000;

	// Push the SPSR (system mode, all interrupts disabled)
	*(--context->sp) = 0xdf;
}

void init_threads(void) {
    int i;
    for (i=0; i<NTHREADS; i++) {
        threads[i].next = i == NTHREADS-1 ? NULL : &threads[i+1];
        threads[i].waitsFor = NULL;
        threads[i].msg = NULL;
        threads[i].placeholders = 0;
        threads[i].thread_no = i;
	    thread_init(i);
    }
    if (&__ram_end - &_end < STACKSIZE)
        panic("Not enough stack for startup/idle thread");
    threadI.next = NULL;
    threadI.thread_no = -1;
    threadI.waitsFor = NULL;
    threadI.msg = NULL;
    threadI.placeholders = 0;
    threadI.context.cookie = &_end;
    *threadI.context.cookie = context_cookie;
}

void init_rts(int argc, char **argv) {
    PROTECT(1);
    TIMERINIT();
    gcInit();
    addRootScanner(&timerQscanner);
    init_threads();
    current_thread->msg = &msg0;
    TIMERGET(msg0.baseline);
}
