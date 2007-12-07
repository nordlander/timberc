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
  POLY a;
  POLY b;
};

struct TUP3 {
  POLY a;
  POLY b;
  POLY c;
};

struct TUP4 {
  POLY a;
  POLY b;
  POLY c;
  POLY d;
};

struct LIST {};
enum {_NIL,_CONS};

struct NIL {};

struct CONS {
  POLY a;
  LIST b;
};

struct FloatBox {
  Float Value;
};

struct TimeBox {
  Time Value;
};

struct IntBox {
  Int Value;
};

struct Msg {
  Int (*Code)(Msg);
  AbsTime baseline;
  AbsTime deadline;
  Msg next;
};

struct Array {
  Int size;
  POLY elems[];
};

typedef struct Array *Array;

UNITTYPE ASYNC(Msg, Time, Time);
PID      LOCK(PID);
UNITTYPE UNLOCK(PID);
POLY     RAISE(Int);

Array primListArray(LIST);
Array primConstArray(Int,POLY);
Array primCloneArray(Array,Int);

#endif
