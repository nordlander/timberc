#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <setjmp.h>
#include <signal.h>
#include <sys/time.h>
#include <string.h>
#include "rts.h"
#include "timber.h"


#define NTHREADS        5
#define STACKSIZE       0x40000                 // words

#define SLEEP()         sigsuspend(&enabled_mask)
#define DISABLE(prev)   sigprocmask(SIG_SETMASK, &disabled_mask, prev)
#define ENABLE(mask)    sigprocmask(SIG_SETMASK, mask, NULL)

#if defined(__APPLE__)
#  if defined(__i386__)
#    if defined(__MAC_OS_X_VERSION_10_5)
#      define SETSTACK(buf,s,a) {(*(buf))[9] = (int)(a) + (s) - 64;}
#    else
#      define SETSTACK(buf,s,a) { (((struct sigcontext *) (buf))->sc_ebp) = (int)(a) + (s) - 64; \
                                  (((struct sigcontext *) (buf))->sc_esp) = (int)(a) + (s) - 64; }
#    endif
#  elif defined(__ppc__)
#    if defined(__DARWIN_UNIX3)
#      define SETSTACK(buf,s,a) { (((struct __darwin_sigcontext *) (buf))->__sc_onstack) = (int)(a) + (s) - 64; }
#    else
#      define SETSTACK(buf,s,a) { (((struct sigcontext *) (buf))->sc_onstack) = (int)(a) + (s) - 64; }
#    endif
#  endif
#elif   defined(__linux__)
#define SETSTACK(buf,s,a) { ((buf)->__jmpbuf[JB_SP]) = (int)(a) + (s) - 4; }
#elif   defined(__NetBSD__)
#define SETSTACK(buf,s,a) { ((buf)[2]) = (int)(a) + (s) - 4; }
#elif   defined(__FreeBSD__)
#define SETSTACK(buf,s,a) { ((buf)->_jb[2]) = (int)(a) + (s) - 4; }
#elif   defined(__qnx__)
#define SETSTACK(buf,s,a) { ((buf)->__jmpbuf_un.__savearea[11]) = (int)(a) + (s) - 4; }
#endif

#define TDELTA          1
#define TIMERGET(x)     { gettimeofday(&x, NULL); }
#define TIMERSET(x,now) { if (x) { \
                                struct itimerval t; \
                                t.it_value = (x)->baseline; \
                                SUB(t.it_value, now); \
                                t.it_interval.tv_sec = 0; \
                                t.it_interval.tv_usec = 0; \
                                setitimer( ITIMER_REAL, &t, NULL); \
                          } \
                        }

#define LESS(a,b)       ( ((a).tv_sec <  (b).tv_sec) || (((a).tv_sec == (b).tv_sec) && ((a).tv_usec <  (b).tv_usec)) )
#define LESSEQ(a,b)     ( ((a).tv_sec <= (b).tv_sec) || (((a).tv_sec == (b).tv_sec) && ((a).tv_usec <= (b).tv_usec)) )
#define ADD(a,t)        { (a).tv_usec += (t) % 1000000; \
                          if ((a).tv_usec >= 1000000) { \
                                  (a).tv_usec -= 1000000; \
                                  (a).tv_sec += 1; \
                          } \
                          (a).tv_sec += (t) / 1000000; \
                        }
#define SUB(a,b)        { (a).tv_usec -= (b).tv_usec; \
                          if ((a).tv_usec < 0) { \
                                  (a).tv_usec += 1000000; \
                                  (a).tv_sec -= 1; \
                          } \
                          (a).tv_sec -= (b).tv_sec; \
                        }

#define INF             0x7fffffff


sigset_t disabled_mask, enabled_mask;


// Last resort -----------------------------------------------------------------------------------

void panic(char *str) {
        DISABLE(NULL);
        fprintf(stderr, "Timber RTS panic: %s. Quitting...\n", str);
        exit(1);
}


// Thread management --------------------------------------------------------------------------------

struct Thread {
        Thread next;             // for use in linked lists
        Msg msg;                 // message under execution
        PID waitsFor;            // deadlock detection link
        jmp_buf context;         // machine state
};

Object ObjInit          = { NULL, NULL };

struct Msg msg0         = { NULL, 0, { 0, 0 }, { INF, 0 }, NULL };

struct Thread threads[NTHREADS];
struct Thread thread0   = { NULL, NULL, NULL, 0 };         // the idle process
struct Thread thread1   = { &thread0, &msg0, NULL, 1 };    // initial process & primary workhorse (default stack)

Msg savedMsg            = NULL;
Msg msgQ                = NULL;
Msg timerQ              = NULL;

Thread threadPool       = threads;
Thread activeStack      = &thread1;
Thread current          = &thread1;


// Memory management --------------------------------------------------------------------------------

#include "gc.c"


// GCINFO definitions for the built-in types -----------------------------------------------------

#include "timber.c"


// Queue management ------------------------------------------------------------------------------

void enqueueByDeadline(Msg p, Msg *queue) {
        Msg prev = NULL, q = *queue;
        while (q && LESS(q->deadline, p->deadline)) {
                prev = q;
                q = q->next;
        }
        p->next = q;
        if (prev == NULL)
                *queue = p;
        else
                prev->next = p;
}

void enqueueByBaseline(Msg p, Msg *queue) {
        Msg prev = NULL, q = *queue;
        while (q && LESS(q->baseline, p->baseline)) {
                prev = q;
                q = q->next;
        }
        p->next = q;
        if (prev == NULL)
                *queue = p;
        else
                prev->next = p;
}

Msg dequeue(Msg *queue) {
        Msg m = *queue;
        if (m)
                *queue = m->next;
        else
                panic("Empty queue");
        return m;
}

void push(Thread t, Thread *stack) {
        t->next = *stack;
        *stack = t;
}

Thread pop(Thread *stack) {
        Thread t = *stack;
        *stack = t->next;
        return t;
}


// Context switching ----------------------------------------------------------------------------

void dispatch( Thread next ) {
    if (setjmp( current->context ) == 0) {
        current = next;
        longjmp( next->context, 1 );
    }
}

void idle(void) {
        ENABLE(&enabled_mask);
        while (1) {
                SLEEP();
        }
}

void INTERRUPT_PROLOGUE() {
        savedMsg = current->msg;
        current->msg = &msg0;
        TIMERGET(msg0.baseline);
}

void INTERRUPT_EPILOGUE() {
        current->msg = savedMsg;
        Msg topMsg = activeStack->msg;
        if (msgQ && threadPool && ((!topMsg) || LESS(msgQ->deadline, topMsg->deadline))) {
                push(pop(&threadPool), &activeStack);
                dispatch(activeStack);
        }
}

void timer_handler(int signo) {
        INTERRUPT_PROLOGUE();
        while (timerQ && LESSEQ(timerQ->baseline, msg0.baseline))
                enqueueByDeadline( dequeue(&timerQ), &msgQ );
        TIMERSET(timerQ, msg0.baseline);
        INTERRUPT_EPILOGUE();
}

void run(void) {
        while (1) {
                Msg this = current->msg = dequeue(&msgQ);
                ENABLE(&enabled_mask);
                this->Code(this);
                DISABLE(NULL);
                current->msg = NULL;
                Msg oldMsg = activeStack->next->msg;
                
                if (!msgQ || (oldMsg && LESS(oldMsg->deadline, msgQ->deadline))) {
                        push(pop(&activeStack), &threadPool);
                        Thread t = activeStack;                     // can't be NULL, may be &thread0
                        while (t->waitsFor) 
                                t = t->waitsFor->ownedBy;
                        dispatch(t);
                }
        }
}


// Major primitives ---------------------------------------------------------------------

UNITTYPE ASYNC( Msg m, Time bl, Time dl ) {
        AbsTime now;
        TIMERGET(now);

        m->baseline = current->msg->baseline;
        if (bl >= 0) {
                ADD(m->baseline, bl);
                if (LESS(m->baseline, now))
                        m->baseline = now;
        }
        if (dl >= 0) {
                m->deadline = m->baseline;
                if (dl == 0) {
                        m->deadline.tv_sec = INF;
                        m->deadline.tv_usec = 0;
                } else
                        ADD(m->deadline, dl);
        } else
                m->deadline = current->msg->deadline;
        
        sigset_t previous_mask;
        DISABLE(&previous_mask);
        if (LESS(now, m->baseline)) {
                TIMERQ_PROLOGUE();
                Msg oldTimerQ = timerQ;
                enqueueByBaseline(m, &timerQ);
                if (timerQ != oldTimerQ)
                        TIMERSET(timerQ, now);
                TIMERQ_EPILOGUE();
        } else {
                enqueueByDeadline(m, &msgQ);
        }
        ENABLE(&previous_mask);
}


PID LOCK( PID to ) {
        GC_PROLOGUE(to);
        sigset_t previous_mask;
        DISABLE(&previous_mask);
        Thread t = to->ownedBy;
        if (t) {                                                // "to" is already locked
                while (t->waitsFor)
                        t = t->waitsFor->ownedBy;
                if (t == current)                               // deadlock
                        panic("Deadlock");
                if (to->wantedBy)
                        to->wantedBy->waitsFor = NULL;
                to->wantedBy = current;
                current->waitsFor = to;
                dispatch(t);
        }
        to->ownedBy = current;
        ENABLE(&previous_mask);
        return to;
}

UNITTYPE UNLOCK( PID to ) {
        sigset_t previous_mask;
        DISABLE(&previous_mask);
        to->ownedBy = NULL;
        Thread t = to->wantedBy;
        if (t) {                                                // we have run on someone's behalf
                to->wantedBy = NULL;
                t->waitsFor = NULL;
                dispatch(t);
        }
        ENABLE(&previous_mask);
        GC_EPILOGUE(to);
}

void init_threads(void) {
        WORD stacksize = (((STACKSIZE-1) / pagesize) + 1) * pagesize;
        ADDR adr;
        int i;

        for (i=0; i<NTHREADS-1; i++)
                threads[i].next = &threads[i+1];
        threads[NTHREADS-1].next = NULL;
    
        for (i=0; i<NTHREADS; i++) {
                if (setjmp( threads[i].context ))
                        run();
                if (!(adr = allocwords(stacksize)))
                        panic("Cannot allocate stack memory");
                if (mprotect(adr, pagesize, PROT_NONE))
                        panic("Cannot protect end-of-stack page");
                SETSTACK( &threads[i].context, stacksize, adr );
                threads[i].waitsFor = NULL;
        }

        thread0.next = NULL;
        thread0.waitsFor = NULL;
        thread0.msg = NULL;
        if (setjmp( thread0.context ))
                idle();
        if (!(adr = allocwords(pagesize)))
                panic("Cannot allocate idle stack memory");
        SETSTACK( &thread0.context, pagesize, adr);
                
        TIMERGET(msg0.baseline);
        thread1.next = &thread0;
        thread1.waitsFor = NULL;
        thread1.msg = &msg0;

        activeStack = &thread1;
}


// Exception handling ----------------------------------------------------------------------------------

POLY RAISE(Int err) {
        panic("Unhandled exception");
}

// String marshalling ----------------------------------------------------------------------------------

LIST getStr(char *p) {
        if (!*p)
                return (LIST)_NIL;
        CONS n0; NEW(CONS, n0, sizeof(struct CONS));
        CONS n = n0;
        n->a = (POLY)(Int)*p++;
        while (*p) {
                NEW(LIST, n->b, sizeof(struct CONS));
                n = (CONS)n->b;
                n->a = (POLY)(Int)*p++;
        }
        n->b = (LIST)_NIL;
        return (LIST)n0;
}


// Arrays ---------------------------------------------------------------------------------------------

#include "arrays.c"


// Environment object ---------------------------------------------------------------------------------

#include "env.c"


// Initialization -------------------------------------------------------------------------------------

void init_rts(int argc, char **argv) {
        sigemptyset(&enabled_mask);
        sigemptyset(&disabled_mask);
        sigaddset(&disabled_mask, SIGALRM);
        sigaddset(&disabled_mask, SIGIO);
        DISABLE(NULL);
        
        gcinit();
        init_threads();
        init_env(argc, argv);

        struct sigaction act;
        act.sa_handler = timer_handler;
        act.sa_flags = 0;
        sigemptyset( &act.sa_mask );
        sigaction( SIGALRM, &act, NULL );        
}

