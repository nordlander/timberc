#ifndef RTS_H_
#define RTS_H_

#include <stddef.h>
#include <sys/time.h>
//#include <setjmp.h>
#include <pthread.h>

typedef int WORD;
typedef WORD *ADDR;

#define Int int
#define Float float
#define Char char
#define Bool char
#define FALSE char          // alias for singleton type
#define TRUE char           // alias for singleton type
#define UNITTYPE char
#define UNITTERM char       // alias for singleton type
#define POLY ADDR
#define PID ADDR
#define BITS8 unsigned char
#define BITS16 unsigned short
#define BITS32 unsigned int

union FloatCast {
    float f;
    POLY p;
};

#define POLY2Float(x)   ((union FloatCast )(x)).f
#define Float2POLY(x)   ((union FloatCast )(x)).p

#define ZEROBITS        0
#define ORBITS(a,b)     (a) | (b)
#define SETBIT(n)       (1 << n)
#define COPYBIT(x,m,n)  (((x >> m) & 1) << n)

/*
#define SEC(x)          ((x)*1000000)
#define MILLISEC(x)     ((x)*1000)
#define MICROSEC(x)     (x)
*/

struct Msg;

typedef struct Ref *Ref;
struct Ref {
    WORD *GCINFO;
    pthread_mutex_t mut;
    POLY STATE;
};

#define STATEOF(ref)    (((ADDR)(ref))+WORDS(sizeof(struct Ref)))

void INITREF(Ref);

extern WORD __GC__Ref[];

typedef struct timeval AbsTime;

struct Time {
  WORD *GCINFO;
  Int sec;
  Int usec;
};

typedef struct Time *Time;

extern WORD __GG__Time[] ;


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

#define NEW(t,addr,words)       { ADDR top,stop; \
                                  do { addr = (t)hp; stop = lim; top = (ADDR)addr+(words); } \
                                  while (!CAS(addr,top,&hp)); \
                                  if (top>=stop) addr = (t)force((words),(ADDR)addr); }

#define CURRENT()               ((Thread)pthread_getspecific(current_key))

/*
#define TMIN(a,b)               ((a) < (b) ? (a) : (b) )
#define TPLUS(a,b)              ((a) + (b))
#define TMINUS(a,b)             ( (a) > (b) ? (a) - (b) : 0 )
*/


extern ADDR hp, lim;
extern pthread_key_t current_key;

ADDR force(WORD, ADDR);
void pruneStaticHeap();

void init_rts(int, char**);

Time sec(Int c);
Time millisec(Int x);
Time microsec(Int x);
Int secOf(Time t);
Int microsecOf (Time t);

#define Inherit ((Time)0)
#define Infinity ((Time)1)

#define INHERIT 0
#define TIME_INFINITY 1


Time primTimeMin(Time t1, Time t2);
Time primTimePlus(Time t1, Time t2);
//Time primTimeMinus(Time t1, Time t2);

Bool primTimeEQ(Time t1, Time t2);
Bool primTimeNE(Time t1, Time t2);
Bool primTimeLT(Time t1, Time t2);
Bool primTimeLE(Time t1, Time t2);
Bool primTimeGT(Time t1, Time t2);
Bool primTimeGE(Time t1, Time t2);

#endif
