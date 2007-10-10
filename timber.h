#ifndef TIMBER_H_
#define TIMBER_H_
#include <stdio.h>
#include <stdlib.h>

#define Int int
#define Float float
#define Char char
#define Bool char
#define UNITTYPE char
#define POLY void*
#define Time int
#define PID void*
#define INFINITY 0
#define NEW malloc

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
struct FloatBox;
typedef struct TimeBox *TimeBox;
struct Msg;
typedef struct Msg *Msg;


enum {_UNITTERM};

enum {_FALSE, _TRUE};
    
struct TUP2 {
  POLY a;
  POLY b;
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
  Time Baseline;
  Time Deadline;
};

UNITTYPE ASYNC(Msg, Time, Time);
UNITTYPE LOCK(PID);
UNITTYPE UNLOCK(PID);

#endif
