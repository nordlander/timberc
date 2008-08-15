
#include "timber.h"

struct S_Timer {
    WORD *gcinfo;
    Thread ownedBy;
    Thread wantedBy;
    AbsTime start;
};

typedef struct S_Timer *S_Timer;

struct T_Timer;
typedef struct T_Timer *T_Timer;

struct T_Timer {
  WORD *gcinfo;
  Msg (*reset) (T_Timer, Time, Time);
  Time (*sample) (T_Timer, POLY);
  S_Timer self;
};

struct Msg_S_Timer;
typedef struct Msg_S_Timer *Msg_S_Timer;

struct Msg_S_Timer {
    WORD *gcinfo;
    UNITTYPE (*Code) (Msg_S_Timer);
    AbsTime baseline;
    AbsTime deadline;
    Msg next;
    S_Timer self;
};

static WORD __GC__Msg_S_Timer[] = {WORDS(sizeof(struct Msg_S_Timer)), WORDS(offsetof(struct Msg_S_Timer, self)), 0};

static WORD __GC__T_Timer[] = {WORDS(sizeof(struct T_Timer)), WORDS(offsetof(struct T_Timer,self)), 0};
   
static WORD __GC__S_Timer[] = {WORDS(sizeof(struct S_Timer)), 0};



static UNITTYPE reset_f(Msg_S_Timer this) {
  S_Timer self;
  self = (S_Timer)LOCK((PID)this->self);
  self->start = current->msg->baseline;
  UNLOCK((PID)self);
  return _UNITTERM;
}

static Msg reset_fun(S_Timer self, Time bl, Time dl) {
  Msg_S_Timer res;
  NEW(Msg_S_Timer,res,WORDS(sizeof(struct Msg_S_Timer)));
  SETGCINFO(res,__GC__Msg_S_Timer);
  res->Code = reset_f;
  res->self = self;
  ASYNC((Msg)res,bl,dl);
  return (Msg)res;
}

static Time sample_fun(S_Timer self, POLY x) {
  S_Timer self1 = (S_Timer)LOCK((PID)self);
  AbsTime now;
  now = current->msg->baseline;
  SUB(now,self1->start);
  UNLOCK((PID)self1);
  Time res;
  res= now.tv_sec * 1000000 + now.tv_usec;
  return res;
}

static Msg reset_sel(T_Timer this, Time bl, Time dl) {
  return reset_fun(this->self,bl,dl);
}

static Time sample_sel(T_Timer this, POLY x) {
  return sample_fun(this->self,x);
}

TIMERTYPE primTIMERTERM(POLY x) {
  S_Timer self;
  NEW(S_Timer,self,WORDS(sizeof(struct S_Timer)));
  SETGCINFO(self,__GC__S_Timer);
  self->ownedBy = (Thread)0;
  self->wantedBy = (Thread)0;
  self->start = current->msg->baseline;
  T_Timer res;
  NEW(T_Timer,res,sizeof(struct T_Timer));
  SETGCINFO(res,__GC__T_Timer);
  res->reset = reset_sel;
  res->sample = sample_sel;
  res->self = self;
  return (TIMERTYPE)res;
}

Int secOf(Time t) {return t / 1000000;}
Int microsecOf(Time t) {return t % 1000000; }
