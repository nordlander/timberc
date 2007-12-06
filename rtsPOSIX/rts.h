#ifndef RTS_H_
#define RTS_H_

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
        Thread ownedBy;
        Thread wantedBy;
};

typedef struct Object Object;
typedef Object *PID;

extern Object ObjInit;

#define WORDS(bytes)        (((bytes)+sizeof(WORD)-1)/sizeof(WORD))
#define ISBLACK(a)          (a >= base2 && a < scanp)

#if defined(__APPLE__)
#include <libkern/OSAtomic.h>
#define CAS(old,new,mem)    OSAtomicCompareAndSwap32((WORD)old,(WORD)new,(ADDR)mem)
#endif

#define NEW(t,addr,size)    { ADDR top; do { addr = (t)hp; top = (ADDR)addr+WORDS(size); } while (!CAS(addr,top,&hp)); \
                              if (top>=lim) addr = (t)force(WORDS(size)); }
#define NEW2(addr,size)     { ADDR top; do { addr = hp2; top = (ADDR)addr + WORDS(size); } while (!CAS(addr,top,&hp2)); \
                              if (top >= lim2) addr = force2(WORDS(size)); }

#define GCINFO(addr)        ((ADDR)addr[0])
#define PROLOGUE(obj)       { if (!ISSTATIC(GCINFO(obj))) obj = GCINFO(obj); }
#define EPILOGUE(obj)       { if (hp2) { \
                                 if (obj==scanning) scanning = 0; \
                                 if (obj==copying) copying = 0; \
                                 if (ISBLACK(obj)) { ADDR a; NEW2(a,1); a[0] = (WORD)obj; } } }     // write barrier

#define TMIN(a,b)           ( (a) > 0 && (a) < (b) ? (a) : (b) )
#define TPLUS(a,b)          ( (a) > 0 ? (a) + (b) : (b) )
#define TMINUS(a,b)         ( (a) > (b) ? (a) - (b) : 0 )

extern ADDR hp, lim, hp2, lim2, base2, scanp;
extern volatile ADDR scanning, copying;

ADDR force(WORD);


//void putStr(LIST);
//LIST getStr(char *);

void init_rts(int, char**);

#endif
