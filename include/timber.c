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

WORD __GC__CONS[]       = {
        HEAD(CONS),     OFF(CONS, a),   OFF(CONS, b),   0,
        HEAD(CONS),                     OFF(CONS, b),   0,0
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

WORD __GC__Ref[]        = {WORDS(sizeof(struct Ref)), GC_MUT, 0};       // sole pointer field "STATE" is custom handled by the gc

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
