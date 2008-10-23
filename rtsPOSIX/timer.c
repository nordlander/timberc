
#include "timber.h"

struct S_Timer {
    WORD *GCINFO;
    AbsTime start;
};

typedef struct S_Timer *S_Timer;

struct T_Timer;
typedef struct T_Timer *T_Timer;

struct T_Timer {
  WORD *GCINFO;
  UNITTYPE (*reset) (T_Timer, Int);
  Time (*sample) (T_Timer, Int);
  Ref self;
};


static WORD __GC__T_Timer[] = {WORDS(sizeof(struct T_Timer)), GC_STD, WORDS(offsetof(struct T_Timer,self)), 0};
   
static WORD __GC__S_Timer[] = {WORDS(sizeof(struct S_Timer)), GC_STD, 0};

static WORD __GC__Time[]    = {WORDS(sizeof(struct Time)), GC_STD, 0};


Time sec(Int c) {
  Time res;
  NEW(Time,res,WORDS(sizeof(struct Time)));
  res->GCINFO = __GC__Time;
  res->sec = c;
  res->usec = 0;
  return res;
}

Time millisec(Int c) {
  Time res;
  NEW(Time,res,WORDS(sizeof(struct Time)));
  res->GCINFO = __GC__Time;
  res->sec = c / 1000;
  res->usec = 1000 * (c % 1000);
  return res;
}

Time microsec(Int c) {
  Time res;
  NEW(Time,res,WORDS(sizeof(struct Time)));
  res->GCINFO = __GC__Time;
  res->sec = c / 1000000;
  res->usec = c % 1000000;
  return res;
}

Int secOf(Time t) {
  switch ((Int)t) {
  case INHERIT: panic("secOf Inherit");
  case TIME_INFINITY: panic("secOf infinity");
  default: return t->sec;
  }
}

Int microsecOf(Time t) {
  switch ((Int)t) {
  case INHERIT: panic("microsecOf Inherit");
  case TIME_INFINITY: panic("microsecOf infinity");
  default: return t->usec;
  }
}



Time primTimePlus(Time t1, Time t2) {
  Time res;
  switch ((Int)t1) {
  case INHERIT: return t2;
  case TIME_INFINITY: return Infinity;
  default: 
    switch ((Int)t2) {
    case INHERIT: return t1;
    case TIME_INFINITY: return Infinity;
    default:
      NEW(Time,res,WORDS(sizeof(struct Time)));
      res->GCINFO = __GC__Time;
      res->usec = t1->usec + t2->usec;
      res->sec = t1->sec + t2->sec;
      if (res->usec >= 1000000) {
	res->usec -= 1000000;
	res->sec += 1;
      }
      return res;
    }
  }
}

Time primTimeMin(Time t1, Time t2) {
  switch ((Int)t1) {
  case INHERIT: 
  case TIME_INFINITY:return t2;
  default:
    switch ((Int)t2) {
    case INHERIT: 
    case TIME_INFINITY: return t1;
    default: 
      if (t1->sec < t2->sec || (t1->sec == t2->sec && t1->usec < t2->usec))
	return t1;
      else
	return t2;
    }
  }
}


Time primTimeMinus(Time t1, Time t2) {
  Time res;
  switch ((Int)t1) {
  case INHERIT: panic("primTimeMinus Inherit");
  case TIME_INFINITY: 
    switch((Int) t2) {
    case INHERIT: panic("primTimeMinus Inherit");
    case TIME_INFINITY: panic("infinity - infinity");
    default: return Infinity;
    }      
  default:
    switch ((Int)t2) {
    case INHERIT: panic("primTimeMinus Inherit");
    case TIME_INFINITY:
      NEW(Time,res,WORDS(sizeof(struct Time)));
      res->GCINFO = __GC__Time;
      res->sec = 0;
      res->usec = 0;
      return res;
    default:
      NEW(Time,res,WORDS(sizeof(struct Time)));
      res->GCINFO = __GC__Time;
      res->usec = t1->usec - t2->usec;
      if (res->usec < 0) {
	res->usec += 1000000;
	t1->sec -= 1;
      }
      res->sec = t1->sec - t2->sec;
      if (res->sec < 0) res->sec = 0;
      return res;
    }
  }
}

Bool primTimeEQ(Time t1, Time t2) {
  switch ((Int)t1) {
  case INHERIT: panic("primTimeEQ Inherit");
  case TIME_INFINITY: switch((Int)t2) {
    case INHERIT: panic("primTimeEQ Inherit");
    case TIME_INFINITY: return 1;
    default:
      return 0;
    }
  default:
    switch ((Int)t2) {
    case INHERIT: panic("primTimeEQ Inherit");
    case TIME_INFINITY: return 0;
    default:
      return (t1->sec == t2->sec && t1->usec == t2->usec);
    }
  }
}

Bool primTimeNE(Time t1, Time t2) {
  switch ((Int)t1) {
  case INHERIT: panic("primTimeNE Inherit");
  case TIME_INFINITY: switch((Int)t2) {
    case INHERIT: panic("primTimeNE Inherit");
    case TIME_INFINITY: return 0;
    default:
      return 1;
    }
  default:
    switch ((Int)t2) {
    case INHERIT: panic("primTimeNE Inherit");
    case TIME_INFINITY: return 1;
    default:
      return (t1->sec != t2->sec || t1->usec != t2->usec);
    }
  }
}

Bool primTimeLT(Time t1, Time t2) {
  switch ((Int)t1) {
  case INHERIT: panic("primTimeLT Inherit");
  case TIME_INFINITY: switch((Int)t2) {
    case INHERIT: panic("primTimeLT Inherit");
    default:
      return 0;
    }
  default:
    switch ((Int)t2) {
    case INHERIT: panic("primTimeLT Inherit");
    case TIME_INFINITY: return 1;
    default:
      return (t1->sec < t2->sec || (t1->sec==t2->sec && t1->usec<t2->usec));
    }
  }
}
Bool primTimeLE(Time t1, Time t2) {
  switch ((Int)t1) {
  case INHERIT: panic("primTime InheritLE");
  case TIME_INFINITY: switch((Int)t2) {
    case INHERIT: panic("primTime InheritLE");
    default:
      return 0;
    }
  default:
    switch ((Int)t2) {
    case INHERIT: panic("primTime InheritLE");
    case TIME_INFINITY: return 1;
    default:
      return (t1->sec < t2->sec || (t1->sec==t2->sec && t1->usec<=t2->usec));
    }
  }
}

Bool primTimeGT(Time t1, Time t2) {
  return primTimeLT(t2,t1);
}

Bool primTimeGE(Time t1, Time t2) {
  return primTimeLE(t2,t1);
}

static UNITTYPE reset_fun(Ref self, Int x) {
  self = (Ref)LOCK((PID)self);
  ((S_Timer)STATEOF(self))->start = CURRENT()->msg->baseline;
  UNLOCK((PID)self);
  return (UNITTYPE)0;
}

static Time sample_fun(Ref self, Int x) {
  self = (Ref)LOCK((PID)self);
  AbsTime now;
  now = CURRENT()->msg->baseline;
  SUB(now,((S_Timer)STATEOF(self))->start);
  UNLOCK((PID)self);
  Time res;
  NEW(Time,res,WORDS(sizeof(struct Time)));
  res->GCINFO = __GC__Time;
  res->sec = now.tv_sec;
  res->usec = now.tv_usec;
  return res;
}

static UNITTYPE reset_sel(T_Timer this,Int x) {
  return reset_fun(this->self,x);
}

static Time sample_sel(T_Timer this, Int x) {
  return sample_fun(this->self,x);
}

TIMERTYPE primTIMERTERM(Int x) {
  Ref self;
  NEW(Ref,self,WORDS(sizeof(struct Ref))+WORDS(sizeof(struct S_Timer)));
  INITREF(self);
  ((S_Timer)STATEOF(self))->GCINFO = __GC__S_Timer;
  ((S_Timer)STATEOF(self))->start = CURRENT()->msg->baseline;
  T_Timer res;
  NEW(T_Timer,res,WORDS(sizeof(struct T_Timer)));
  res->GCINFO = __GC__T_Timer;
  res->reset = reset_sel;
  res->sample = sample_sel;
  res->self = self;
  return (TIMERTYPE)res;
}
