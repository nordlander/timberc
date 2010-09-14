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

#define HEAD(X)         WORDS(sizeof(struct X)), GC_STD
#define OFF(X,Y)        WORDS(offsetof(struct X, Y))

WORD __GC__TUP2[]       = 
        {
        HEAD(TUP2),      OFF(TUP2, a),  OFF(TUP2, b),   0,
        HEAD(TUP2),                     OFF(TUP2, b),   0,0,
        HEAD(TUP2),      OFF(TUP2, a),                  0,0,
        HEAD(TUP2),                                     0,0,0
        };
                                                 
WORD __GC__TUP3[]       = 
        {
        HEAD(TUP3),      OFF(TUP3, a),  OFF(TUP3, b),   OFF(TUP3, c),   0,
        HEAD(TUP3),                     OFF(TUP3, b),   OFF(TUP3, c),   0,0,
        HEAD(TUP3),      OFF(TUP3, a),                  OFF(TUP3, c),   0,0,
        HEAD(TUP3),                                     OFF(TUP3, c),   0,0,0,
        HEAD(TUP3),      OFF(TUP3, a),  OFF(TUP3, b),                   0,0,
        HEAD(TUP3),                     OFF(TUP3, b),                   0,0,0,
        HEAD(TUP3),      OFF(TUP3, a),                                  0,0,0,
        HEAD(TUP3),                                                     0,0,0,0
        };
                                                 
WORD __GC__TUP4[]       = 
        {                        
        HEAD(TUP4),      OFF(TUP4, a),  OFF(TUP4, b),   OFF(TUP4, c),   OFF(TUP4, d),   0,
        HEAD(TUP4),                     OFF(TUP4, b),   OFF(TUP4, c),   OFF(TUP4, d),   0,0,
        HEAD(TUP4),      OFF(TUP4, a),                  OFF(TUP4, c),   OFF(TUP4, d),   0,0,
        HEAD(TUP4),                                     OFF(TUP4, c),   OFF(TUP4, d),   0,0,0,
        HEAD(TUP4),      OFF(TUP4, a),  OFF(TUP4, b),                   OFF(TUP4, d),   0,0,
        HEAD(TUP4),                     OFF(TUP4, b),                   OFF(TUP4, d),   0,0,0,
        HEAD(TUP4),      OFF(TUP4, a),                                  OFF(TUP4, d),   0,0,0,
        HEAD(TUP4),                                                     OFF(TUP4, d),   0,0,0,0,
        HEAD(TUP4),      OFF(TUP4, a),  OFF(TUP4, b),   OFF(TUP4, c),                   0,0,
        HEAD(TUP4),                     OFF(TUP4, b),   OFF(TUP4, c),                   0,0,0,
        HEAD(TUP4),      OFF(TUP4, a),                  OFF(TUP4, c),                   0,0,0,
        HEAD(TUP4),                                     OFF(TUP4, c),                   0,0,0,0,
        HEAD(TUP4),      OFF(TUP4, a),  OFF(TUP4, b),                                   0,0,0,
        HEAD(TUP4),                     OFF(TUP4, b),                                   0,0,0,0,
        HEAD(TUP4),      OFF(TUP4, a),                                                  0,0,0,0,
        HEAD(TUP4),                                                                     0,0,0,0,0
        };

WORD __GC__TUPLE[] = {WORDS(sizeof(struct TUPLE)),GC_TUPLE,0};

WORD __GC__CLOS1[] = {WORDS(sizeof(struct CLOS1)),GC_STD,0};
WORD __GC__CLOS2[] = {WORDS(sizeof(struct CLOS2)),GC_STD,0};
WORD __GC__CLOS3[] = {WORDS(sizeof(struct CLOS3)),GC_STD,0};

WORD __GC__CLOS[]  = {WORDS(sizeof(struct CLOS)),GC_STD,0};

WORD __GC__CONS[]       = {
        HEAD(CONS),     OFF(CONS, hd),  OFF(CONS, tl),  0,
        HEAD(CONS),                     OFF(CONS, tl),  0,0
        };

WORD __GC__LEFT[]       = {
        HEAD(LEFT),     OFF(LEFT, a),   0,
        HEAD(LEFT),                     0,0
        };

WORD __GC__RIGHT[]      = {
        HEAD(RIGHT),    OFF(RIGHT, a),  0,
        HEAD(RIGHT),                    0,0
        };

WORD __GC__Timer[]      = {
        HEAD(Timer),    0
        };

/*
struct Msg {
  Int (*Code)(Msg);
  AbsTime baseline;
  AbsTime deadline;
  Msg next;
};
*/
WORD __GC__Msg[]        = {WORDS(sizeof(struct Msg)), GC_STD, 0};       // sole pointer field "next" is custom handled by the gc

WORD __GC__Ref[]        = {WORDS(sizeof(struct Ref)), GC_MUT, OFF(Ref,STATE), 0,
                           WORDS(sizeof(struct Ref)), GC_MUT, 0,              0 };

/*
struct Array {
  Int size;
  POLY elems[];
};
*/
WORD __GC__Array0[]     = {WORDS(sizeof(struct Array)), GC_ARRAY, 0};       // flag 0 => node contains all pointers

WORD __GC__Array1[]     = {WORDS(sizeof(struct Array)), GC_ARRAY, 1};     // flag 1 => node contains all scalars


POLY primRefl(BITS32 polytag, POLY in) {
        return in;
}

struct T1 {
	POLY GCINFO;
	POLY (*Code) (CLOS1, POLY);
	CLOS1 coerce;
};
typedef struct T1 *T1;
WORD __GC__T1[] = {WORDS(sizeof(struct T1)),GC_STD, OFF(T1, coerce), 0};

struct T2 {
	POLY GCINFO;
	POLY (*Code) (CLOS1, POLY);
	CLOS1 coerce;
	CLOS1 cl;
};
typedef struct T2 *T2;
WORD __GC__T2[] = {WORDS(sizeof(struct T2)),GC_STD, OFF(T2, coerce), OFF(T2, cl), 0};

POLY takeDummy(CLOS1 this, POLY dummy) {
	CLOS1 cl = ((T2)this)->cl;
	CLOS1 coerce = ((T2)this)->coerce;
	POLY obj = cl->Code(cl, (POLY)0);
	POLY obj2 = coerce->Code(coerce, obj);
	return obj2;
}

POLY takeClass(CLOS1 this, POLY cl) {
	T2 x;
	NEW(T2, x, WORDS(sizeof(struct T2)));
	x->GCINFO = __GC__T2;
	x->Code = takeDummy;
	x->coerce = ((T1)this)->coerce;
	x->cl = (CLOS1)cl;
	return (POLY)x;
}

CLOS1 primClassRefl(BITS32 polytag, CLOS1 coerce) {
	T1 x;
	NEW(T1, x, WORDS(sizeof(struct T1)));
	x->GCINFO = __GC__T1;
	x->Code = takeClass;
	x->coerce = coerce;
	return (CLOS1)x;
}

// String marshalling ----------------------------------------------------------------------------------

LIST getStr(char *p) {
        if (!*p)
                return (LIST)0;
        CONS n0; NEW(CONS, n0, WORDS(sizeof(struct CONS)));
        n0->GCINFO = __GC__CONS;
        CONS n = n0;
        n->hd = (POLY)(Int)*p++;
        while (*p) {
	        NEW(LIST, n->tl, WORDS(sizeof(struct CONS)));
                n = (CONS)n->tl;
                n->GCINFO = __GC__CONS;
                n->hd = (POLY)(Int)*p++;
        }
        n->tl = (LIST)0;
        return (LIST)n0;
}

Int strEq (LIST s1, LIST s2) {
  Char c1, c2;
  while(1) {
    switch ((Int)s1) {
    case 0: 
      return ((Int)s2==0);
    default: 
      switch ((Int)s2) {
      case 0:
	return 0;
      default:
	c1 = (Int)((CONS)s1)->hd;
	c2 = (Int)((CONS)s2)->hd;
        if (c1!=c2) return 0;
	s1 = ((CONS)s1)->tl;
	s2 = ((CONS)s2)->tl;
      }
    }
  }
}

// Mutable lists ---------------------------------------------------------------------------------------------

WORD __GC__MUTLIST[]  = {WORDS(sizeof(struct MUTLIST)),GC_STD, OFF(MUTLIST, anchor), 0};

MUTLIST MUTLISTINIT(BITS32 polytag) {
	MUTLIST x;
	NEW(MUTLIST, x, WORDS(sizeof(struct MUTLIST)));
	x->GCINFO = __GC__MUTLIST;
	x->anchor = (LIST)0;
	x->current = &x->anchor;
	return x;
}

UNIT MUTLISTEXTEND(BITS32 polytag, MUTLIST x, POLY e) {
	CONS tmp;
	NEW(CONS, tmp, WORDS(sizeof(struct CONS)));
	tmp->GCINFO = __GC__CONS;
	tmp->hd = e;
	tmp->tl = (LIST)0;
	*x->current = (LIST)tmp;
	x->current = &tmp->tl;
	return (UNIT)0;
}
