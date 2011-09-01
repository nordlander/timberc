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

#ifndef TIMBER_H_
#define TIMBER_H_
#include <math.h>
#include <sys/time.h>
#include <pthread.h>
#include "config.h"


#define Int int
#define Float float
#define Char char
#define Bool char
#define TUP0 char

#define UNIT TUP0
#define POLY ADDR

#define OID Ref
#define BITS8 unsigned char
#define BITS16 unsigned short
#define BITS32 unsigned int

typedef struct timeval AbsTime;

typedef int WORD;
typedef WORD *ADDR;



struct TUP2;
typedef struct TUP2 *TUP2;
struct TUP3;
typedef struct TUP3 *TUP3;
struct TUP4;
typedef struct TUP4 *TUP4;
struct TUPLE;
typedef struct TUPLE *TUPLE;

struct CLOS;
typedef struct CLOS *CLOS;

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

struct Time;
typedef struct Time *Time;
struct World;
typedef struct World *World;
struct Array;
typedef struct Array *Array;
struct TIMERTYPE;
typedef struct TIMERTYPE *TIMERTYPE;
struct MUTLIST;
typedef struct MUTLIST *MUTLIST;
struct Msg;
typedef struct Msg *Msg;
struct Ref;
typedef struct Ref *Ref;
struct Thread;
typedef struct Thread *Thread;


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

struct TUPLE {
    WORD *GCINFO;
    WORD size;
    POLY elems[];
};
extern WORD __GC__TUPLE[];

struct CLOS {
    POLY GCINFO;
    void (*Code) (void);
};
extern WORD __GC__CLOS[];

struct LIST {
    WORD *GCINFO;
};

struct CONS {
  WORD *GCINFO;
  POLY hd;
  LIST tl;
};
extern WORD __GC__CONS[];

struct EITHER {
  WORD *GCINFO;
  Int Tag;
};

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

struct Time {
    WORD *GCINFO;
    Int sec;
    Int usec;
};
extern WORD __GG__Time[] ;

struct Msg {
  WORD *GCINFO;
  UNIT (*Code)(Msg,OID);
  OID Obj;
  Thread sender;
  Msg next;
  AbsTime baseline;
  AbsTime deadline;
};
extern WORD __GC__Msg[];

struct Array {
  WORD *GCINFO;
  Int size;
  POLY elems[];
};
extern WORD __GC__Array0[];
extern WORD __GC__Array1[];

struct TIMERTYPE {
  WORD *GCINFO;
  UNIT (*reset) (TIMERTYPE, Int);
  Time (*sample) (TIMERTYPE, Int);
};
extern WORD __GC__TIMERTYPE[];

struct MUTLIST {
	WORD *GCINFO;
	LIST anchor;
	LIST *current;
};
extern WORD __GC__MUTLIST[];

struct Ref {
    WORD *GCINFO;
#if (TARGET==POSIX)
    pthread_mutex_t mut;
#endif
    POLY STATE;
};
extern WORD __GC__Ref[];


#define STATEOF(ref)    (((ADDR)(ref))+WORDS(sizeof(struct Ref)))

void INITREF(OID);




union FloatCast {
    float f;
    POLY p;
};

#define POLY2Float(x)   ((union FloatCast )(x)).f
#define Float2POLY(x)   ((union FloatCast )((float)x)).p

#define primIntToChar (Char)
#define primCharToInt (Int)


Msg   ASYNC(Msg, Time, Time);
OID   LOCK(OID);
UNIT  UNLOCK(OID);
void  RAISE(Int);

POLY  Raise(BITS32, Int);

Time sec(Int c);
Time millisec(Int x);
Time microsec(Int x);
Int secOf(Time t);
Int microsecOf (Time t);

#define Inherit ((Time)0)

Time primTimePlus(Time t1, Time t2);
Time primTimeMinus(Time t1, Time t2);

Bool primTimeEQ(Time t1, Time t2);
Bool primTimeNE(Time t1, Time t2);
Bool primTimeLT(Time t1, Time t2);
Bool primTimeLE(Time t1, Time t2);
Bool primTimeGT(Time t1, Time t2);
Bool primTimeGE(Time t1, Time t2);

Array primListArray(BITS32,LIST);
Array primUniArray(BITS32,Int,POLY);
Array EmptyArray(BITS32,Int);
Array CloneArray(BITS32,Array,Int);
Array primUpdateArray(BITS32,Array,Int,POLY);

Array CYCLIC_BEGIN(Int, Int);
void  CYCLIC_UPDATE(Array, Int, ADDR stop);
void  CYCLIC_END(Array, ADDR stop);

POLY  primRefl(BITS32,POLY);
CLOS  primClassRefl(BITS32, CLOS);


TIMERTYPE primTIMERTERM(Int x);
UNIT  ABORT(BITS32,Msg msg,Ref x);

LIST  primShowFloat(Float x);
LIST  getStr(char *p) ;
Int   strEq (LIST s1, LIST s2) ;


MUTLIST MUTLISTINIT(BITS32);
UNIT    MUTLISTEXTEND(BITS32,MUTLIST,POLY);
#define MUTLISTEXTRACT(polytag,x)  (x->anchor)
#define MUTLISTAPPEXTRACT(polytag,x,e) (*x->current = e, x->anchor)

#define primSHIFTRA8(a,b)  ((signed char)(a) >> (b))
#define primSET8(a,b)      ((a) | (1 << (b)))
#define primCLR8(a,b)      ((a) & ~(1 << (b)))
#define primTST8(a,b)      (((a) & (1 << (b))) != 0)

#define primSHIFTRA16(a,b) ((signed short)(a) >> (b))
#define primSET16(a,b)     ((a) | (1 << (b)))
#define primCLR16(a,b)     ((a) & ~(1 << (b)))
#define primTST16(a,b)     (((a) & (1 << (b))) != 0)

#define primSHIFTRA32(a,b) ((signed int)(a) >> (b))
#define primSET32(a,b)     ((a) | (1 << (b)))
#define primCLR32(a,b)     ((a) & ~(1 << (b)))
#define primTST32(a,b)     (((a) & (1 << (b))) != 0)


#define WORDS(bytes)            (((bytes)+sizeof(WORD)-1)/sizeof(WORD))
#define BYTES(words)            ((words)*sizeof(WORD))


#if defined(HAVE_OSX_ATOMICS)
#include <libkern/OSAtomic.h>
#define CAS(old,new,mem)        OSAtomicCompareAndSwap32((WORD)(old),(WORD)(new),(ADDR)(mem))
#else
#if defined(HAVE_BUILTIN_ATOMIC) 
#define CAS(old,new,mem)        __sync_bool_compare_and_swap((mem),(WORD)(old),(WORD)(new))
#else
#error "Can not define CAS on your architecture."
#endif
#endif

#define NEW(t,addr,words)       { ADDR top,stop; \
                                  do { addr = (t)hp; stop = lim; top = ((ADDR)addr)+(words); } \
                                  while (!CAS(addr,top,&hp)); \
                                  if (top>=stop) { addr = (t)force((words),(ADDR)addr<stop?(ADDR)addr:0);} }

// Note: soundness of the spin-loop above depends on the invariant that lim is never changed unless hp also changes.


extern ADDR hp, lim;
ADDR force(WORD, ADDR);



#endif
