#ifndef RTS_H_
#define RTS_H_


#include <stddef.h>

typedef int WORD;
typedef WORD *ADDR;

#define Int int
#define Float float
#define Char char
#define Bool char
#define UNITTYPE char
#define POLY void*
#define Time int

#define AbsTime Time

struct Thread;
typedef struct Thread *Thread;

struct Object {
};
typedef struct Object Object;
typedef Object *PID;

extern Object ObjInit;


#define WORDS(bytes)                    (((bytes)+sizeof(WORD)-1)/sizeof(WORD))

#define NEW(t,lhs,words)  lhs = (t)hp; hp += (words); if (hp >= lim) lhs = (t)force(words);

extern ADDR hp, lim;
ADDR force(WORD);

#define SEC(x)          ((x)*1000000)
#define MILLISEC(x)     ((x)*1000)
#define MICROSEC(x)     (x)


#define TMIN(a,b)               ( (a) > 0 && (a) < (b) ? (a) : (b) )
#define TPLUS(a,b)              ( (a) > 0 ? (a) + (b) : (b) )
#define TMINUS(a,b)             ( (a) > (b) ? (a) - (b) : 0 )


void init_rts(int, char**);

#endif
