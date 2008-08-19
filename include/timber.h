#ifndef TIMBER_H_
#define TIMBER_H_

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
struct FloatBox;
typedef struct FloatBox *FloatBox;
struct IntBox;
typedef struct IntBox *IntBox;
/*
struct TimeBox;
typedef struct TimeBox *TimeBox;
*/
struct Msg;
typedef struct Msg *Msg;

enum {_UNITTERM};

enum {_FALSE, _TRUE};
    
struct TUP2 {
  WORD *gcinfo;
  POLY a;
  POLY b;
};
extern WORD __GC__TUP2[];

struct TUP3 {
  WORD *gcinfo;
  POLY a;
  POLY b;
  POLY c;
};
extern WORD __GC__TUP3[];

struct TUP4 {
  WORD *gcinfo;
  POLY a;
  POLY b;
  POLY c;
  POLY d;
};
extern WORD __GC__TUP4[];

struct LIST {};
enum {_NIL,_CONS};

struct NIL {};

struct CONS {
  WORD *gcinfo;
  POLY a;
  LIST b;
};
extern WORD __GC__CONS[];

struct EITHER {
  WORD *gcinfo;
  Int Tag;
};
enum {_LEFT,_RIGHT};

extern WORD __GC__EITHER[];
struct LEFT {
  WORD *gcinfo;
  Int Tag;
  POLY a;
};
extern WORD __GC__LEFT[];

struct RIGHT {
  WORD *gcinfo;
  Int Tag;
  POLY a;
};
extern WORD __GC__RIGHT[];

struct FloatBox {
  WORD *gcinfo;
  Float Value;
};
extern WORD __GC__FloatBox[];
/*
struct TimeBox {
  WORD *gcinfo;
  Time Value;
};
extern WORD __GC__TimeBox[];
*/
struct IntBox {
  WORD *gcinfo;
  Int Value;
};
extern WORD __GC__IntBox[];

struct Msg {
  WORD *gcinfo;
  Int (*Code)(Msg);
  AbsTime baseline;
  AbsTime deadline;
  Msg next;
};
extern WORD __GC__Msg[];

struct Array {
  WORD *gcinfo;
  Int size;
  POLY elems[];
};
extern WORD __GC__Array[];

typedef struct Array *Array;

struct Timer;
typedef struct Timer *TIMERTYPE;


struct Timer {
  WORD *gcinfo;
  UNITTYPE (*reset) (TIMERTYPE, POLY);
  Time (*sample) (TIMERTYPE, POLY);
};

extern WORD __GC__Timer[];

UNITTYPE ASYNC(Msg, Time, Time);
PID      LOCK(PID);
UNITTYPE UNLOCK(PID);
POLY     RAISE(Int);

Array primListArray(LIST);
Array primUniArray(Int,POLY);
Array EmptyArray(Int);
Array CloneArray(Array,Int);
Array primUpdateArray(Array,Int,POLY);

Array CYCLIC_BEGIN(Int, Int);
void  CYCLIC_UPDATE(Array, Int);
void  CYCLIC_END(Array);

POLY primRefl(POLY in);

TIMERTYPE primTIMERTERM(POLY x);
UNITTYPE ABORT(Msg msg,POLY x);

#endif
