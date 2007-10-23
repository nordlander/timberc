#ifndef TIMBER_H_
#define TIMBER_H_

typedef int WORD;
typedef WORD *ADDR;

extern ADDR hp, lim;
ADDR force(WORD);

#define WORDS(bytes)      (((bytes)+sizeof(WORD)-1)/sizeof(WORD))
#define NEW(t,lhs,size)   lhs = (t)hp; hp += WORDS(size); if (hp >= lim) lhs = (t)force(WORDS(size));

#define Int int
#define Float float
#define Char char
#define Bool char
#define UNITTYPE char
#define POLY void*
#define Time int
#define PID void*
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

POLY RAISE(Int);

struct Array {
  Int size;
  POLY elems[];
};

typedef struct Array *Array;

Array primListArray(LIST);
Array primConstArray(Int,POLY);
Array primCloneArray(Array,Int);
#endif
