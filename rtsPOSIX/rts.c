#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <signal.h>
#include <sys/time.h>
#include <string.h>
#include "rts.h"
#include "timber.h"


#define MAXTHREADS      8

#define SLEEP()         sigsuspend(&enabled_mask)
#define DISABLE(mutex)  pthread_mutex_lock(&mutex)
#define ENABLE(mutex)   pthread_mutex_unlock(&mutex)


#define TDELTA          1
#define TIMERGET(x)     gettimeofday(&x, NULL)
#define TIMERSET(x,now) { struct itimerval t; \
                          t.it_value = (x); \
                          SUB(t.it_value, now); \
                          t.it_interval.tv_sec = 0; \
                          t.it_interval.tv_usec = 0; \
                          setitimer( ITIMER_REAL, &t, NULL); \
                        }

#define LESS(a,b)       ( ((a).tv_sec < (b).tv_sec) || (((a).tv_sec == (b).tv_sec) && ((a).tv_usec <  (b).tv_usec)) )
#define LESSEQ(a,b)     ( ((a).tv_sec < (b).tv_sec) || (((a).tv_sec == (b).tv_sec) && ((a).tv_usec <= (b).tv_usec)) )
#define ADD(a,t)        { (a).tv_usec += (t->usec); \
                          if ((a).tv_usec >= 1000000) { \
                                  (a).tv_usec -= 1000000; \
                                  (a).tv_sec += 1; \
                          } \
                          (a).tv_sec += (t->sec); \
                        }
#define SUB(a,b)        { (a).tv_usec -= (b).tv_usec; \
                          if ((a).tv_usec < 0) { \
                                  (a).tv_usec += 1000000; \
                                  (a).tv_sec -= 1; \
                          } \
                          (a).tv_sec -= (b).tv_sec; \
                        }

#define INF             0x7fffffff


// Thread management --------------------------------------------------------------------------------

struct Thread;
typedef struct Thread *Thread;

struct Thread {
        Thread next;            // for use in linked lists
        Msg msg;                // message under execution
        int prio;
        pthread_t id;
        int index;
        pthread_cond_t trigger;
        int placeholders;       // for use during cyclic data construction
};

Msg msgQ                = NULL;
Msg timerQ              = NULL;

Thread runQ             = NULL;
Thread sleepQ           = NULL;

int nactive             = 0;
int nthreads            = 0;

struct Thread threads[MAXTHREADS];

pthread_mutex_t rts;

pthread_mutexattr_t glob_mutexattr;
pthread_mutexattr_t obj_mutexattr;

sigset_t all_sigs;

pthread_key_t current_key;

int prio_min, prio_max;

#define NCORES          2
#define PRIO(t)         (t ? t->prio : )


Thread newThread(Msg m, int prio, void *(*fun)(void *), int stacksize) {
    Thread t = NULL;
    if (nthreads < MAXTHREADS) {
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
        pthread_create(&t->id, &attr, fun, t);
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

UNITTYPE ABORT(BITS32 polytag, Msg m, Ref dummy){
    m->Code = NULL;
    ADDR info;
    do {
        info = IND0((ADDR)m);
        if (ISFORWARD(info))
            ((Msg)info)->Code = NULL;
    } while (info != IND0((ADDR)m));
    return (UNITTYPE)0;
}



// Thread management ------------------------------------------------------------------------

int midPrio(Thread prev, Thread next) {
    int left  = (prev ? prev->prio : prio_max-1);
    int right = (next ? next->prio : prio_min+1);
    return right + ((left - right) / 2);
}


void *run(void*);

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

int activate(Msg m) {
    int count = 0;
    Thread prev = NULL, t = runQ;
    AbsTime dl = m->deadline;
    while (count < NCORES && t && LESS(t->msg->deadline, dl)) {
        count++;
        prev = t;
        t = t->next;
    }
    if (count >= NCORES)
        return 0;
    Thread new = getThread(m, midPrio(prev,t));
    if (new == NULL)
        return 0;
    new->next = t;
    if (prev == NULL)
        runQ = new;
    else
        prev->next = new;
    nactive++;
    // fprintf(stderr, "Worker thread %d activated (%d)\n", (int)new, nactive);
    return 1;
}

void deactivate(Thread t) {
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
    // fprintf(stderr, "Worker thread %d deactivated (%d)\n", (int)t, nactive);
}

void *run(void *arg) {
    Thread current = (Thread)arg;
    pthread_setspecific(current_key, current);
    struct sched_param param;
    param.sched_priority = current->prio;
    pthread_setschedparam(current->id, SCHED_RR, &param);
    // fprintf(stderr, "Worker thread %d started\n", (int)current);
    DISABLE(rts);
    while (1) {
        Msg this = current->msg;

        ENABLE(rts);
        Int (*code)(Msg) = this->Code;
        if (code)
            code(this);
        DISABLE(rts);

        deactivate(current);
        while (msgQ && !(msgQ->Code))
            msgQ = msgQ->next;
        if (msgQ) {
            activate(msgQ);
            msgQ = msgQ->next;
        } else {
            pthread_cond_wait(&current->trigger, &rts);
        }
        if (STARTGC())
                gcStart();
    }
}


// Major primitives ---------------------------------------------------------------------

UNITTYPE ASYNC( Msg m, Time bl, Time dl ) {
    DISABLE(rts);

    AbsTime now;
    TIMERGET(now);
    Thread current = CURRENT();
    // fprintf(stderr, "Working thread %d in ASYNC\n", (int)current);
    m->baseline = current->msg->baseline;
    switch ((Int)bl) {
	    case INHERIT: break;
        case TIME_INFINITY:
	        m->baseline.tv_sec = INF;
	        m->baseline.tv_usec = 0;
	        break;
        default:
            ADD(m->baseline, bl);
            if (LESS(m->baseline, now))
                m->baseline = now;
    }
    switch((Int)dl) {
	    case INHERIT: 
	        m->deadline = current->msg->deadline;
            break;
	    case TIME_INFINITY:
	        m->deadline.tv_sec = INF;
	        m->deadline.tv_usec = 0;
	        break;
	    default:
	        m->deadline = m->baseline;
            ADD(m->deadline, dl);
	}
        
    if (LESS(now, m->baseline)) {           //  TIMERQ_PROLOGUE();
        enqueueByBaseline(m, &timerQ);
        timerQdirty = 1;
        if (timerQ == m)
            TIMERSET(m->baseline, now);     //  TIMERQ_EPILOGUE();
    } else if (!activate(m))
        enqueueByDeadline(m, &msgQ);

    ENABLE(rts);
    return (UNITTYPE)0;
}


void INITREF( Ref obj ) {
        obj->GCINFO = __GC__Ref;
        pthread_mutex_init(&obj->mut, &obj_mutexattr);
        obj->STATE = (ADDR)STATEOF(obj);                              // actually unused, but keep it clean
}

PID LOCK( PID to ) {
    Ref r = (Ref)to;
    pthread_mutex_lock(&(r->mut));
    GC_PROLOGUE(to);
    if (to != (PID)r) {
        pthread_mutex_lock(&(((Ref)to)->mut));
        pthread_mutex_unlock(&(r->mut));
    }
    return to;
}

UNITTYPE UNLOCK( PID to ) {
    GC_EPILOGUE(to);
    pthread_mutex_unlock(&(((Ref)to)->mut));
    return (UNITTYPE)0;
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


// Arrays ---------------------------------------------------------------------------------------------

#include "arrays.c"

// Primitive timer class ------------------------------------------------------------------------------

#include "timer.c"

// Environment object ---------------------------------------------------------------------------------

#include "env.c"

// Show Float -----------------------------------------------------------------------------------------

#include "float.c"

// timerQ handling ------------------------------------------------------------------------------------

int timerQdirty;

void *timerHandler(void *arg) {
    Thread current = (Thread)arg;
    struct sched_param param;
    param.sched_priority = current->prio;
    pthread_setschedparam(current->id, SCHED_RR, &param);

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
        while (timerQ && LESSEQ(timerQ->baseline, now)) {
            Msg m = dequeue(&timerQ);
            if (m->Code) {
                if (!activate(m))
                    enqueueByDeadline(m, &msgQ);
            }
        }
        if (timerQ)
            TIMERSET(timerQ->baseline, now);
        ENABLE(rts);
    }
}

void scanTimerQ() {
        timerQdirty = 0;
        DISABLE(rts);
        if (timerQ) {
                timerQ = (Msg)copy((ADDR)timerQ);
                ENABLE(rts);
                DISABLE(rts);
                Msg m = timerQ, next = m->next;
                while (next) {
                        m->next = (Msg)copy((ADDR)next);
                        ENABLE(rts);
                        DISABLE(rts);
                        m = m->next;
                        next = m->next;
                }
        }
        ENABLE(rts);
}


// Initialization -------------------------------------------------------------------------------------

void init_rts(int argc, char **argv) {
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
    sigemptyset(&all_sigs);
    sigaddset(&all_sigs, SIGALRM);
    sigaddset(&all_sigs, SIGSELECT);
    pthread_sigmask(SIG_BLOCK, &all_sigs, NULL);
    
    DISABLE(rts);
    
    gcInit();
    envInit(argc, argv);
    newThread(NULL, prio_max, timerHandler, pagesize);
    
    ENABLE(rts);
}

