WORD __GC__TUP2[] = {WORDS(sizeof(struct TUP2)), WORDS(offsetof(struct TUP2, a)), WORDS(offsetof(struct TUP2, b)), 0};

WORD __GC__TUP3[] = {WORDS(sizeof(struct TUP3)), WORDS(offsetof(struct TUP3, a)), WORDS(offsetof(struct TUP3, b)), 
                     WORDS(offsetof(struct TUP3, c)), 0};

WORD __GC__TUP4[] = {WORDS(sizeof(struct TUP4)), WORDS(offsetof(struct TUP4, a)), WORDS(offsetof(struct TUP4, b)), 
                     WORDS(offsetof(struct TUP4, c)), WORDS(offsetof(struct TUP4, d)), 0};

// Note: tuple types will eventually be implemented by arrays...


WORD __GC__CONS[] = {WORDS(sizeof(struct CONS)), WORDS(offsetof(struct CONS, a)), WORDS(offsetof(struct CONS, b)), 0};

WORD __GC__EITHER[] = {WORDS(sizeof(struct EITHER)), 0};

WORD __GC__LEFT[] = {WORDS(sizeof(struct LEFT)), WORDS(offsetof(struct LEFT, a)), 0};

WORD __GC__RIGHT[] = {WORDS(sizeof(struct RIGHT)), WORDS(offsetof(struct RIGHT, a)), 0};

WORD __GC__FloatBox[] = {WORDS(sizeof(struct FloatBox)), 0};

WORD __GC__TimeBox[] = {WORDS(sizeof(struct TimeBox)), 0};

WORD __GC__IntBox[] = {WORDS(sizeof(struct IntBox)), 0};

/*
struct Msg {
  Int (*Code)(Msg);
  AbsTime baseline;
  AbsTime deadline;
  Msg next;
};
*/
WORD __GC__Msg[] = {WORDS(sizeof(struct Msg)), 0};      // sole pointer field "next" is custom handled by the gc

/*
struct Array {
  Int size;
  POLY elems[];
};
*/
WORD __GC__Array[] = {0, 1};                            // zero size => dynamic, flag 1 => node contains all pointers

POLY primRefl(POLY in) {
        return in;
}