#include <stdlib.h>
#include <stdio.h>
#include "timber.h"
#include "rts.h"


// Memory management --------------------------------------------------------------------------------

#include "gc.c"

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
    gcinit();
}

