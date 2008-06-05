#ifndef RTS_H_
#define RTS_H_

#include <stddef.h>
#include <sys/time.h>
#include <setjmp.h>

typedef int WORD;
typedef WORD *ADDR;

#define Int int
#define Float float
#define Char char
#define Bool char
#define UNITTYPE char
#define POLY void*
#define Time int

#define SEC(x)          ((x)*1000000)
#define MILLISEC(x)     ((x)*1000)
#define MICROSEC(x)     (x)

struct Thread;
typedef struct Thread *Thread;

struct Msg;

typedef struct Object Object;
typedef Object *PID;


struct Thread {
        Thread next;            // for use in linked lists
        struct Msg *msg;        // message under execution
        PID waitsFor;           // deadlock detection link
        WORD visit_flag;        // for use during cyclic data construction
        int placeholders;       // for use during cyclic data construction
        jmp_buf context;        // machine state
};


typedef struct timeval AbsTime;

struct Object {
        WORD *gcinfo;
        Thread ownedBy;
        Thread wantedBy;
};

extern Thread current;

#define WORDS(bytes)            (((bytes)+sizeof(WORD)-1)/sizeof(WORD))
#define BYTES(words)            ((words)*sizeof(WORD))


#if defined(__APPLE__)
#include <libkern/OSAtomic.h>
#define CAS(old,new,mem)        OSAtomicCompareAndSwap32((WORD)(old),(WORD)(new),(ADDR)(mem))
#endif

#if defined(__linux__)
#include <atomic_ops.h>
#define CAS(old,new,mem)        AO_compare_and_swap((AO_t*)(mem),(WORD)(old),(WORD)(new))
#endif

#define NEW(t,addr,words)       { ADDR top; do { addr = (t)hp; top = (ADDR)addr+(words); } while (!CAS(addr,top,&hp)); \
                                  if (top>=lim) addr = (t)force(words); }

#define SETGCINFO(n,info)       { (n)->gcinfo = (ADDR)((WORD)(info) | current->visit_flag); }

#define TMIN(a,b)               ( (a) > 0 && (a) < (b) ? (a) : (b) )
#define TPLUS(a,b)              ( (a) > 0 ? (a) + (b) : (b) )
#define TMINUS(a,b)             ( (a) > (b) ? (a) - (b) : 0 )

extern ADDR hp, lim;

ADDR force(WORD);
void pruneStaticHeap();

void init_rts(int, char**);

#define ACTION CLOS_145_POSIX
#endif
