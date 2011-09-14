// The Timber compiler <timber-lang.org>
// 
// Copyright 2008-2009 Johan Nordlander <nordland@csee.ltu.se>
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
// 
// 3. Neither the names of the copyright holder and any identified
//    contributors, nor the names of their affiliations, may be used to 
//    endorse or promote products derived from this software without 
//    specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
// OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
// OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
// STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
// ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <signal.h>
#include <sys/time.h>
#include <string.h>
#include "rts.h"

#define MAXTHREADS      12          // Static maximum

#define TDELTA          1
#define TIMERSET(x,now) { struct itimerval t; \
                          t.it_value = (x); \
                          ABS_SUB(t.it_value, now); \
                          t.it_interval.tv_sec = 0; \
                          t.it_interval.tv_usec = 0; \
                          setitimer( ITIMER_REAL, &t, NULL); \
                        }


// Thread management --------------------------------------------------------------------------------

Int NCORES              = 0;
Int NTHREADS            = 0;


Msg msgQ                = NULL;
Msg timerQ              = NULL;

Thread runQ             = NULL;
Thread sleepQ           = NULL;

int nactive             = 0;
int nthreads            = 0;

struct Msg msg0         = { NULL, NULL, NULL, NULL, NULL, { 0, 0 }, { 0, 0 } };

struct Thread thread0   = { NULL, &msg0, 0,  };

struct Thread threads[MAXTHREADS];

pthread_mutex_t rts;
pthread_mutexattr_t glob_mutexattr;
pthread_mutexattr_t obj_mutexattr;

sigset_t all_sigs;

pthread_key_t current_key;

int prio_min, prio_max;


Thread newThread(Msg m, int prio, void (*fun)(Thread), int stacksize) {
    Thread t = NULL;
    if (nthreads < NTHREADS) {
        t = &threads[nthreads++];
        t->msg = m;
        t->prio = prio;
        t->placeholders = 0;
        t->index = nthreads;
        pthread_attr_t attr;
        pthread_attr_init(&attr);
        pthread_attr_setinheritsched(&attr, PTHREAD_EXPLICIT_SCHED);
        if (stacksize > 0)
                pthread_attr_setstacksize(&attr, BYTES(stacksize));
        pthread_cond_init(&t->trigger, NULL);
        pthread_create(&t->id, &attr, (void*(*)(void*))fun, t);
    }
    return t;
}



// Last resort -----------------------------------------------------------------------------------

void panic(char *str) {
    DISABLE(rts);
    fprintf(stderr, "Timber RTS panic: %s. Quitting...\n", str);
    exit(1);
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


// Thread management ------------------------------------------------------------------------

int midPrio(Thread prev, Thread next) {
    int left  = (prev ? prev->prio : prio_max-1);
    int right = (next ? next->prio : prio_min+1);
    return right + ((left - right) / 2);
}


void run(Thread);

Thread getThread(Msg m, int prio) {
    Thread t = sleepQ;
    if (t) {
        struct sched_param param;
        param.sched_priority = prio;
        sleepQ = t->next;
        t->msg = m;
        pthread_setschedparam(t->id, SCHED_RR, &param);
        pthread_cond_signal(&t->trigger);
    } else
        t = newThread(m, prio, run, 0);
    return t;
}

int activate(Msg m, int force) {
    int count = 0;
    Thread prev = NULL, t = runQ;
    AbsTime dl = m->deadline;

    while (count < NCORES && t && ABS_LT(t->msg->deadline, dl)) {
        count++;
        prev = t;
        t = t->next;
    }
    if (count >= NCORES && !force) {
        // fprintf(stderr, "** Out of cores\n");
        return 0;
    }
    Thread new = getThread(m, midPrio(prev,t));
    if (new == NULL) {
        // fprintf(stderr, "** Out of threads\n");
        return 0;
    }
    new->next = t;
    if (prev == NULL)
        runQ = new;
    else
        prev->next = new;
    nactive++;
    // fprintf(stderr, "++ %d | runQ: ", new->index);
    // Thread th = runQ;
    // while (th) {
    //         fprintf(stderr, "%d ", th->index);
    //         th = th->next;
    // }
    // fprintf(stderr, "\n");
    return 1;
}

void deactivate(Thread t) {
    // fprintf(stderr, "-- %d | runQ: ", t->index);
    if (t == runQ)
        runQ = runQ->next;
    else {
        Thread prev = runQ, q = runQ->next;
        while (q != t) {
            prev = q;
            q = q->next;
        }
        prev->next = q->next;
    }
    t->next = sleepQ;
    sleepQ = t;
    nactive--;
    // Thread th = runQ;
    // while (th) {
    //        fprintf(stderr, "%d ", th->index);
    //        th = th->next;
    // }
    // fprintf(stderr, "\n");
}

void run(Thread current_thread) {
    pthread_setspecific(current_key, current_thread);
    struct sched_param param;
    param.sched_priority = current_thread->prio;
    pthread_setschedparam(current_thread->id, SCHED_RR, &param);
    DISABLE(rts);
    while (1) {
        Msg this = current_thread->msg;
        ENABLE(rts);
        
        this->Obj = LOCK(this->Obj);
        if (this->sender)
            pthread_cond_signal(&this->sender->trigger);
        
        UNIT (*code)(Msg,OID) = this->Code;
        if (code)
            code(this, this->Obj);
        UNLOCK(this->Obj);
            
        DISABLE(rts);
        deactivate(current_thread);

        if (heapLevel(16) > 13)
            gcStart();

        while (msgQ && !(msgQ->Code))
            msgQ = msgQ->next;
        if (msgQ) {
            activate(msgQ, 1);
            msgQ = msgQ->next;
        } else {            
            pthread_cond_wait(&current_thread->trigger, &rts);
        }
    }
}

// Major primitives ---------------------------------------------------------------------

Msg ASYNC( Msg m, Time bl, Time dl ) {
    DISABLE(rts);

    Thread current_thread = CURRENT();
    // fprintf(stderr, "Working thread %d in ASYNC\n", (int)current_thread);

    m->baseline = current_thread->msg->baseline;
    if (bl) {
        ABS_ADD(m->baseline, bl);
        m->sender = NULL;
    } else
        m->sender = current_thread;

    if (dl) {
	    m->deadline = m->baseline;
        ABS_ADD(m->deadline, dl);
	} else if (ABS_LT(m->baseline, current_thread->msg->deadline))
	    m->deadline = current_thread->msg->deadline;
	else
        m->deadline = absInfinity;

    AbsTime now;
    TIMERGET(now);
    if (ABS_LT(now, m->baseline)) {           //  TIMERQ_PROLOGUE();
        enqueueTimerQ(m);
        rootsDirty = 1;
        if (timerQ == m)
            TIMERSET(m->baseline, now);     //  TIMERQ_EPILOGUE();
    } else if (!activate(m,0))
        enqueueMsgQ(m);

    if (!bl)
        pthread_cond_wait(&current_thread->trigger, &rts);

    ENABLE(rts);
    return m;
}


void INITREF( OID obj ) {
        obj->GCINFO = __GC__Ref;
        pthread_mutex_init(&obj->mut, &obj_mutexattr);
        obj->STATE = (ADDR)STATEOF(obj);                              // actually unused, but keep it clean
}

OID LOCK( OID to ) {
    OID r = to;
    pthread_mutex_lock(&(r->mut));
    GC_PROLOGUE(to);
    if (to != r) {
        pthread_mutex_lock(&(to->mut));
        pthread_mutex_unlock(&(r->mut));
    }
    return to;
}

UNIT UNLOCK( OID to ) {
    GC_EPILOGUE(to);
    pthread_mutex_unlock(&(to->mut));
    return (UNIT)0;
}


UNIT ABORT(BITS32 polytag, Msg m, Ref dummy){
    m->Code = NULL;
    ADDR info;
    do {
        info = IND0((ADDR)m);
        if (ISFORWARD(info))
            ((Msg)info)->Code = NULL;
    } while (info != IND0((ADDR)m));
    return (UNIT)0;
}



// Exception handling ----------------------------------------------------------------------------------

void RAISE(Int err) {
        char buf[100];
        sprintf(buf, "Unhandled exception (%d)", err);
        panic(buf);
}

POLY Raise(BITS32 polyTag, Int err) {
        RAISE(err);
        return NULL;
}


// Primitive timer class ------------------------------------------------------------------------------

#include "timer.c"


// timerQ handling ------------------------------------------------------------------------------------

void timerHandler(Thread current_thread) {
    struct sched_param param;
    param.sched_priority = current_thread->prio;
    pthread_setschedparam(current_thread->id, SCHED_RR, &param);

//    pthread_sigmask(SIG_BLOCK, &all_sigs, NULL);
    sigset_t accept;
    sigemptyset(&accept);
    sigaddset(&accept, SIGALRM);
    while (1) {
        int received;
        sigwait(&accept, &received);
        DISABLE(rts);
        AbsTime now;
        TIMERGET(now);
        while (timerQ && ABS_LE(timerQ->baseline, now)) {
            Msg m = timerQ;
            timerQ = timerQ->next;
            if (m->Code) {
                if (!activate(m,0))
                    enqueueMsgQ(m);
            }
        }
        if (timerQ)
            TIMERSET(timerQ->baseline, now);
        ENABLE(rts);
    }
}

void scanTimerQ() {
        DISABLE(rts);
        if (timerQ) {
                timerQ = (Msg)copy((ADDR)timerQ);
                Msg m = timerQ;
                ENABLE(rts);
                DISABLE(rts);
                Msg next = m->next;
                while (next) {
                        m->next = (Msg)copy((ADDR)next);
                        m = m->next;
                        ENABLE(rts);
                        DISABLE(rts);
                        next = m->next;
                }
        }
        ENABLE(rts);
}

struct Scanner timerQscanner = { scanTimerQ, NULL };


// Arrays ---------------------------------------------------------------------------------------------

#include "arrays.c"

// Show Float -----------------------------------------------------------------------------------------

#include "float.c"

// Command line args ----------------------------------------------------------------------------------

int argc0;
char **argv0;

int getArgc() {
    return argc0;
}

char **getArgv() {
    return argv0;
}

// Main thread handling -------------------------------------------------------------------------------

pthread_cond_t sleepVar;
void (*mainContinuation) (void) = NULL;

void runAsMainContinuation(void(*fun)(void)) {
    DISABLE(rts);
    if (mainContinuation)
        panic("Multiple main continuations");
    mainContinuation = fun;
    ENABLE(rts);
}

void startup(Time_Time_to_Msg prog) {
    prog->Code(prog, Inherit, Inherit);
    DISABLE(rts);
    if (!mainContinuation)
        pthread_cond_wait(&sleepVar, &rts);
    if (!mainContinuation)
        panic("Main thread woke up with no continuation");
    ENABLE(rts);
    (*mainContinuation)();
}

// Initialization -------------------------------------------------------------------------------------

Thread runAsSeparateThread(void(*fun)(Thread), Msg m) {
    Thread t = NULL;
    DISABLE(rts);
    t = newThread(m, prio_max, fun, pagesize);
    ENABLE(rts);
    return t;
}

Int getNumberOfProcessors() {
#if defined(HAVE_SYSCONF) && defined(_SC_NPROCESSORS_ONLN)
    Int n = sysconf(_SC_NPROCESSORS_ONLN);
    if (n >= 1)
        return n;
#endif
    return 1;
}

void init_rts(int argc, char **argv) {
    argc0 = argc;
    argv0 = argv;

    pthread_mutexattr_init(&glob_mutexattr);
    pthread_mutexattr_settype(&glob_mutexattr, PTHREAD_MUTEX_NORMAL);
    pthread_mutexattr_setprotocol(&glob_mutexattr, PTHREAD_PRIO_INHERIT);
    pthread_mutex_init(&rts, &glob_mutexattr);
    
    pthread_mutexattr_init(&obj_mutexattr);
    pthread_mutexattr_settype(&obj_mutexattr, PTHREAD_MUTEX_NORMAL);
    pthread_mutexattr_setprotocol(&obj_mutexattr, PTHREAD_PRIO_INHERIT);
    
    prio_min = sched_get_priority_min(SCHED_RR);
    prio_max = sched_get_priority_max(SCHED_RR);
    pthread_key_create(&current_key, NULL);
    pthread_setspecific(current_key, &thread0);
    thread0.id = pthread_self();
    TIMERGET(msg0.baseline);
    msg0.deadline = absInfinity;
    
    sigemptyset(&all_sigs);
    sigaddset(&all_sigs, SIGALRM);
    sigaddset(&all_sigs, SIGSELECT);
    pthread_sigmask(SIG_BLOCK, &all_sigs, NULL);
    
    pthread_cond_init(&sleepVar, NULL);

    DISABLE(rts);
    
    NCORES = getNumberOfProcessors();
    NTHREADS = NCORES * 4;
    if (NTHREADS > MAXTHREADS)
        NTHREADS = MAXTHREADS;
    
    gcInit();
    addRootScanner(&timerQscanner);
    newThread(NULL, prio_max, timerHandler, pagesize);
    
    ENABLE(rts);
}
