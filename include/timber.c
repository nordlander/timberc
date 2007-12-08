WORD __GC__TUP2[] = {sizeof(struct TUP2), offsetof(struct TUP2, a), offsetof(struct TUP2, b), 0};

WORD __GC__TUP3[] = {sizeof(struct TUP3), offsetof(struct TUP3, a), offsetof(struct TUP3, b), 
                     offsetof(struct TUP3, c), 0};

WORD __GC__TUP4[] = {sizeof(struct TUP4), offsetof(struct TUP4, a), offsetof(struct TUP4, b), 
                     offsetof(struct TUP4, c), offsetof(struct TUP4, d), 0};

// Note: tuple types will eventually be implemented by arrays...


WORD __GC__CONS[] = {sizeof(struct CONS), offsetof(struct CONS, a), offsetof(struct CONS, b), 0};

WORD __GC__FloatBox[] = {sizeof(struct FloatBox), 0};

WORD __GC__TimeBox[] = {sizeof(struct TimeBox), 0};

WORD __GC__IntBox[] = {sizeof(struct IntBox), 0};

/*
struct Msg {
  Int (*Code)(Msg);
  AbsTime baseline;
  AbsTime deadline;
  Msg next;
};
*/
WORD __GC__Msg[] = {sizeof(struct Msg), 0};     // field "next" is costom handled by the gc

/*
struct Array {
  Int size;
  POLY elems[];
};
*/
WORD __GC__Array[] = {sizeof(struct Array), 0}; // this is not a correct def, just a temporary placeholder...
