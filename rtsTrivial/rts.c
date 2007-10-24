#include <stdlib.h>
#include <stdio.h>
#include "timber.h"
#include "rts.h"


// Memory management --------------------------------------------------------------------------------

#define allocwords(size)   malloc(size*sizeof(WORD))
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


// String marshalling ----------------------------------------------------------------------------------

#include "timberstrings.c"

// Arrays ---------------------------------------------------------------------------------------------

#include "arrays.c"

// Initialization -------------------------------------------------------------------------------------

void init_rts(void) {
    base = (ADDR)allocwords(HEAPSIZE);
    base[0] = 0;
    hp = base + 1;
    lim = base + HEAPSIZE;
    heapsize = thissize = HEAPSIZE;
}

