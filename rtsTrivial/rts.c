#include <stdlib.h>
#include <stdio.h>
#include "rts.h"
#include "timber.h"


Object ObjInit = { };


// GCINFO definitions for the built-in types -----------------------------------------------------

#include "timber.c"


// Memory management --------------------------------------------------------------------------------

#define allocwords(words)  malloc((words)*sizeof(WORD))
#define HEAPSIZE           0x100000


ADDR base, lim, hp;
WORD heapsize, thissize;


void panic(void) {
        fprintf(stderr, "Timber RTS cannot allocate more memory, quitting...\n");
}

void extend(WORD size) {
    thissize = (size > HEAPSIZE-1 ? size : HEAPSIZE);
    ADDR a = (ADDR)allocwords(thissize);
    if (!a)
        panic();
    base[0] = (WORD)a;
    a[0] = 0;
    base = a;
    lim = a + thissize;
    hp = a + 1;
    heapsize += thissize;
}

ADDR force(WORD size) {                 // Heap overflow in from-space...
    ADDR a;
    extend(size);
    a = hp;
    hp += size;
    return a;
}


// Exception handling ----------------------------------------------------------------------------------

POLY RAISE(Int err) {
    fprintf(stderr, "Unhandled exception no. %d\n", err);
    exit(1);
}


// Stubs for the threading operations (can never be called) --------------------------------------------

UNITTYPE ASYNC(Msg m, Time bl, Time dl) { }
PID      LOCK(PID p) { return p; }
UNITTYPE UNLOCK(PID p) { }

// String marshalling and output -----------------------------------------------------------------------

void putStr(LIST xs) {
  Char x;
  while(1) {
    switch ((Int)xs) {
    case _NIL: 
      putchar('\n');
      return;
    default: 
      x = (Char)(Int)((CONS)xs)->a;
      xs = ((CONS)xs)->b;
      putchar(x);
      break;
    }
  }
}

LIST getStr(char *p) {
    if (!*p)
        return (LIST)_NIL;
    CONS n0; NEW(CONS, n0, sizeof(struct CONS));
    CONS n = n0;
    n->a = (POLY)(Int)*p++;
    while (*p) {
        NEW(LIST, n->b, sizeof(struct CONS));
        n = (CONS)n->b;
        n->a = (POLY)(Int)*p++;
    }
    n->b = (LIST)_NIL;
    return (LIST)n0;
}


// Arrays ---------------------------------------------------------------------------------------------

#include "arrays.c"

// Initialization -------------------------------------------------------------------------------------

void init_rts(int argc, char **argv) {
    base = (ADDR)allocwords(HEAPSIZE);
    base[0] = 0;
    hp = base + 1;
    lim = base + HEAPSIZE;
    heapsize = thissize = HEAPSIZE;
}

