#ifndef TIMBER_H_
#define TIMBER_H_


#define INFINITY 0
#define Inherit -1

struct TUP2;
typedef struct TUP2 *TUP2;
struct LIST;
typedef struct LIST *LIST;
struct NIL;
typedef struct NIL *NIL;
struct CONS;
typedef struct CONS *CONS;
struct FloatBox;
typedef struct FloatBox *FloatBox;
struct IntBox;
typedef struct IntBox *IntBox;
struct TimeBox;
typedef struct TimeBox *TimeBox;
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

struct FloatBox {
  WORD *gcinfo;
  Float Value;
};
extern WORD __GC__FloatBox[];

struct TimeBox {
  WORD *gcinfo;
  Time Value;
};
extern WORD __GC__TimeBox[];

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

UNITTYPE ASYNC(Msg, Time, Time);
PID      LOCK(PID);
UNITTYPE UNLOCK(PID);
POLY     RAISE(Int);

Array primListArray(LIST);
Array primConstArray(Int,POLY);
Array primEmptyArray(Int);
Array primCloneArray(Array,Int);

#endif
