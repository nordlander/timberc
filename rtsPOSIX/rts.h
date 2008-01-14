#ifndef RTS_H_
#define RTS_H_

#include <stddef.h>
#include <sys/time.h>

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

typedef struct timeval AbsTime;

struct Object {
        WORD *gcinfo;
        Thread ownedBy;
        Thread wantedBy;
};

typedef struct Object Object;
typedef Object *PID;

extern Object ObjInit;

#define WORDS(bytes)                    (((bytes)+sizeof(WORD)-1)/sizeof(WORD))


#if defined(__APPLE__)
#include <libkern/OSAtomic.h>
#define CAS(old,new,mem)        OSAtomicCompareAndSwap32((WORD)old,(WORD)new,(ADDR)mem)
#endif

#define NEW(t,addr,words)       { ADDR top; do { addr = (t)hp; top = (ADDR)addr+(words); } while (!CAS(addr,top,&hp)); \
                                  if (top>=lim) addr = (t)force(words); }

#define TMIN(a,b)               ( (a) > 0 && (a) < (b) ? (a) : (b) )
#define TPLUS(a,b)              ( (a) > 0 ? (a) + (b) : (b) )
#define TMINUS(a,b)             ( (a) > (b) ? (a) - (b) : 0 )

extern ADDR hp, lim;

ADDR force(WORD);

void init_rts(int, char**);

#endif
