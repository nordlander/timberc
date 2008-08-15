
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
  UNITTYPE (*reset) (T_Timer, POLY);
  Time (*sample) (T_Timer, POLY);
  S_Timer self;
};


static WORD __GC__T_Timer[] = {WORDS(sizeof(struct T_Timer)), WORDS(offsetof(struct T_Timer,self)), 0};
   
static WORD __GC__S_Timer[] = {WORDS(sizeof(struct S_Timer)), 0};

static WORD __GC__Time[] = {WORDS(sizeof(struct Time)), 0};


Time sec(Int c) {
  Time res;
  NEW(Time,res,sizeof(struct Time));
  SETGCINFO(res,__GC__Time);
  res->sec = c;
  res->usec = 0;
  return res;
}

Time millisec(Int c) {
  Time res;
  NEW(Time,res,sizeof(struct Time));
  SETGCINFO(res,__GC__Time);
  res->sec = c / 1000;
  res->usec = 1000 * (c % 1000);
  return res;
}

Time microsec(Int c) {
  Time res;
  NEW(Time,res,sizeof(struct Time));
  SETGCINFO(res,__GC__Time);
  res->sec = c / 1000000;
  res->usec = c % 1000000;
  return res;
}

Int secOf(Time t) {
  switch ((Int)t) {
  case INHERIT: panic("secOf Inherit");
  case INFINITY: panic("secOf infinity");
  default: return t->sec;
  }
}

Int microsecOf(Time t) {
  switch ((Int)t) {
  case INHERIT: panic("microsecOf Inherit");
  case INFINITY: panic("microsecOf infinity");
  default: return t->usec;
  }
}



Time primTimePlus(Time t1, Time t2) {
  Time res;
  switch ((Int)t1) {
  case INHERIT: return t2;
  case INFINITY: return infinity;
  default: 
    switch ((Int)t2) {
    case INHERIT: return t1;
    case INFINITY: return infinity;
    default:
      NEW(Time,res,sizeof(struct Time));
      SETGCINFO(res,__GC__Time);
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
  case INFINITY:return t2;
  default:
    switch ((Int)t2) {
    case INHERIT: 
    case INFINITY: return t1;
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
  case INFINITY: 
    switch((Int) t2) {
    case INHERIT: panic("primTimeMinus Inherit");
    case INFINITY: panic("infinity - infinity");
    default: return infinity;
    }      
  default:
    switch ((Int)t2) {
    case INHERIT: panic("primTimeMinus Inherit");
    case INFINITY:
      NEW(Time,res,sizeof(struct Time));
      SETGCINFO(res,__GC__Time);
      res->sec = 0;
      res->usec = 0;
      return res;
    default:
      NEW(Time,res,sizeof(struct Time));
      SETGCINFO(res,__GC__Time);
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
  case INFINITY: switch((Int)t2) {
    case INHERIT: panic("primTimeEQ Inherit");
    case INFINITY: return 1;
    default:
      return 0;
    }
  default:
    switch ((Int)t2) {
    case INHERIT: panic("primTimeEQ Inherit");
    case INFINITY: return 0;
    default:
      return (t1->sec == t2->sec && t1->usec == t2->usec);
    }
  }
}

Bool primTimeNE(Time t1, Time t2) {
  switch ((Int)t1) {
  case INHERIT: panic("primTimeNE Inherit");
  case INFINITY: switch((Int)t2) {
    case INHERIT: panic("primTimeNE Inherit");
    case INFINITY: return 0;
    default:
      return 1;
    }
  default:
    switch ((Int)t2) {
    case INHERIT: panic("primTimeNE Inherit");
    case INFINITY: return 1;
    default:
      return (t1->sec != t2->sec || t1->usec != t2->usec);
    }
  }
}

Bool primTimeLT(Time t1, Time t2) {
  switch ((Int)t1) {
  case INHERIT: panic("primTimeLT Inherit");
  case INFINITY: switch((Int)t2) {
    case INHERIT: panic("primTimeLT Inherit");
    default:
      return 0;
    }
  default:
    switch ((Int)t2) {
    case INHERIT: panic("primTimeLT Inherit");
    case INFINITY: return 1;
    default:
      return (t1->sec < t2->sec || (t1->sec==t2->sec && t1->usec<t2->usec));
    }
  }
}
Bool primTimeLE(Time t1, Time t2) {
  switch ((Int)t1) {
  case INHERIT: panic("primTime InheritLE");
  case INFINITY: switch((Int)t2) {
    case INHERIT: panic("primTime InheritLE");
    default:
      return 0;
    }
  default:
    switch ((Int)t2) {
    case INHERIT: panic("primTime InheritLE");
    case INFINITY: return 1;
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

static UNITTYPE reset_fun(S_Timer self, POLY x) {
  self = (S_Timer)LOCK((PID)self);
  self->start = current->msg->baseline;
  UNLOCK((PID)self);
  return _UNITTERM;
}

static Time sample_fun(S_Timer self, POLY x) {
  S_Timer self1 = (S_Timer)LOCK((PID)self);
  AbsTime now;
  now = current->msg->baseline;
  SUB(now,self1->start);
  UNLOCK((PID)self1);
  Time res;
  NEW(Time,res,sizeof(struct Time));
  SETGCINFO(res,__GC__Time);
  res->sec = now.tv_sec;
  res->usec = now.tv_usec;
  return res;
}

static UNITTYPE reset_sel(T_Timer this,POLY x) {
  return reset_fun(this->self,x);
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
