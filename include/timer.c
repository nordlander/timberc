// The Timber compiler <timber-lang.org>
// 
// Copyright 2008-2009 Johan Nordlander <nordland@csee.ltu.se>
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
// 
// 3. Neither the names of the copyright holder and any identified
//    contributors, nor the names of their affiliations, may be used to 
//    endorse or promote products derived from this software without 
//    specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
// OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
// OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
// STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
// ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.


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
    UNIT (*reset) (T_Timer, Int);
    Time (*sample) (T_Timer, Int);
    Ref self;
};


static WORD __GC__T_Timer[] = {WORDS(sizeof(struct T_Timer)), GC_STD, WORDS(offsetof(struct T_Timer,self)), 0};
   
static WORD __GC__S_Timer[] = {WORDS(sizeof(struct S_Timer)), GC_STD, 0};


Time sec(Int c) {
    if (c < 0) c = 0;
    Time res;
    NEW(Time,res,WORDS(sizeof(struct Time)));
    res->GCINFO = __GC__Time;
    res->sec = c;
    res->usec = 0;
    return res;
}

Time millisec(Int c) {
    if (c < 0) c = 0;
    Time res;
    NEW(Time,res,WORDS(sizeof(struct Time)));
    res->GCINFO = __GC__Time;
    res->sec = c / 1000;
    res->usec = 1000 * (c % 1000);
    return res;
}

Time microsec(Int c) {
    if (c < 0) c = 0;
    Time res;
    NEW(Time,res,WORDS(sizeof(struct Time)));
    res->GCINFO = __GC__Time;
    res->sec = c / 1000000;
    res->usec = c % 1000000;
    return res;
}

Time nanosec(Int c) {
    if (c < 0) c = 0;
    Time res;
    NEW(Time,res,WORDS(sizeof(struct Time)));
    res->GCINFO = __GC__Time;
    res->sec = c / 1000000000;
    res->usec = (c % 1000000000) / 1000;
    return res;
}

Int secOf(Time t) {
    return t->sec;
}

Int microsecOf(Time t) {
    return t->usec;
}



Time primTimePlus(Time t1, Time t2) {
    Time res;
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

Time primTimeMinus(Time t1, Time t2) {
    Time res;
    NEW(Time,res,WORDS(sizeof(struct Time)));
    res->GCINFO = __GC__Time;
    if (primTimeLT(t1,t2)) {
        res->sec = 0;
        res->usec = 0;
    } else {
        res->usec = t1->usec - t2->usec;
        res->sec = t1->sec - t2->sec;
        if (res->usec < 0) {
            res->usec += 1000000;
            res->sec -= 1;
        }
    }
    return res;
}

Bool primTimeEQ(Time t1, Time t2) {
    return (t1->sec == t2->sec && t1->usec == t2->usec);
}

Bool primTimeNE(Time t1, Time t2) {
    return (t1->sec != t2->sec || t1->usec != t2->usec);
}

Bool primTimeLT(Time t1, Time t2) {
    return (t1->sec < t2->sec || (t1->sec == t2->sec && t1->usec < t2->usec));
}

Bool primTimeLE(Time t1, Time t2) {
    return (t1->sec < t2->sec || (t1->sec == t2->sec && t1->usec <= t2->usec));
}

Bool primTimeGT(Time t1, Time t2) {
    return (t1->sec > t2->sec || (t1->sec == t2->sec && t1->usec > t2->usec));
}

Bool primTimeGE(Time t1, Time t2) {
    return (t1->sec > t2->sec || (t1->sec == t2->sec && t1->usec >= t2->usec));
}


static UNIT reset_fun(Ref self, Int x) {
    self = (Ref)LOCK((OID)self);
    ((S_Timer)STATEOF(self))->start = CURRENT()->msg->baseline;
    UNLOCK((OID)self);
    return (UNIT)0;
}

static Time sample_fun(Ref self, Int x) {
    self = (Ref)LOCK((OID)self);
    AbsTime now;
    now = CURRENT()->msg->baseline;
    ABS_SUB(now,((S_Timer)STATEOF(self))->start);
    UNLOCK((OID)self);
    Time res;
    NEW(Time,res,WORDS(sizeof(struct Time)));
    res->GCINFO = __GC__Time;
    res->sec = now.tv_sec;
    res->usec = now.tv_usec;
    return res;
}

static UNIT reset_sel(T_Timer this,Int x) {
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
