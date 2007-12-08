#ifndef RTS_H_
#define RTS_H_


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

#define InitObj  { }

#define offsetof(type, member)  ((WORD)(&(((type*)0)->member)))


#define WORDS(bytes)      (((bytes)+sizeof(WORD)-1)/sizeof(WORD))
#define NEW(t,lhs,size)   lhs = (t)hp; hp += WORDS(size); if (hp >= lim) lhs = (t)force(WORDS(size));

extern ADDR hp, lim;
ADDR force(WORD);


void init_rts(int, char**);

#endif
