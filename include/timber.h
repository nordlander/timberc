#ifndef TIMBER_H_
#define TIMBER_H_
#include <math.h>

struct TUP2;
typedef struct TUP2 *TUP2;
struct TUP3;
typedef struct TUP3 *TUP3;
struct TUP4;
typedef struct TUP4 *TUP4;
struct LIST;
typedef struct LIST *LIST;
struct NIL;
typedef struct NIL *NIL;
struct CONS;
typedef struct CONS *CONS;
struct EITHER;
typedef struct EITHER *EITHER;
struct LEFT;
typedef struct LEFT *LEFT;
struct RIGHT;
typedef struct RIGHT *RIGHT;

struct Msg;
typedef struct Msg *Msg;

struct TUP2 {
  WORD *GCINFO;
  POLY a;
  POLY b;
};
extern WORD __GC__TUP2[];

struct TUP3 {
  WORD *GCINFO;
  POLY a;
  POLY b;
  POLY c;
};
extern WORD __GC__TUP3[];

struct TUP4 {
  WORD *GCINFO;
  POLY a;
  POLY b;
  POLY c;
  POLY d;
};
extern WORD __GC__TUP4[];

struct LIST {
    WORD *GCINFO;
};

struct CONS {
  WORD *GCINFO;
  POLY a;
  LIST b;
};
extern WORD __GC__CONS[];

struct EITHER {
  WORD *GCINFO;
  Int Tag;
};

extern WORD __GC__EITHER[];
struct LEFT {
  WORD *GCINFO;
  Int Tag;
  POLY a;
};
extern WORD __GC__LEFT[];

struct RIGHT {
  WORD *GCINFO;
  Int Tag;
  POLY a;
};
extern WORD __GC__RIGHT[];

struct Msg {
  WORD *GCINFO;
  Int (*Code)(Msg);
  AbsTime baseline;
  AbsTime deadline;
  Msg next;
};
extern WORD __GC__Msg[];

struct Array {
  WORD *GCINFO;
  Int size;
  POLY elems[];
};
extern WORD __GC__Array0[];
extern WORD __GC__Array1[];


typedef struct Array *Array;

struct Timer;
typedef struct Timer *TIMERTYPE;


struct Timer {
  WORD *GCINFO;
  UNITTYPE (*reset) (TIMERTYPE, Int);
  Time (*sample) (TIMERTYPE, Int);
};

extern WORD __GC__Timer[];


UNITTYPE ASYNC(Msg, Time, Time);
PID      LOCK(PID);
UNITTYPE UNLOCK(PID);
void     RAISE(Int);

POLY     Raise(BITS32, Int);

Array primListArray(BITS32,LIST);
Array primUniArray(BITS32,Int,POLY);
Array EmptyArray(BITS32,Int);
Array CloneArray(BITS32,Array,Int);
Array primUpdateArray(BITS32,Array,Int,POLY);

Array CYCLIC_BEGIN(Int, Int);
void  CYCLIC_UPDATE(Array, Int, ADDR stop);
void  CYCLIC_END(Array, ADDR stop);

POLY primRefl(BITS32,POLY);

TIMERTYPE primTIMERTERM(Int x);
UNITTYPE ABORT(BITS32,Msg msg,Ref x);

LIST primShowFloat(Float x);
LIST getStr(char *p) ;
Int strEq (LIST s1, LIST s2) ;
#endif
