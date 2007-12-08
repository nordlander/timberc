#include <stdio.h>
#include "rts.h"
#include "timber.h"

void ROOTINIT(void);
extern LIST ROOT(LIST);

LIST getStr(char *);
void putStr(LIST);

int main(int argc, char **argv) {
    init_rts(argc, argv);
    ROOTINIT();
    LIST w = (LIST)_NIL;
    for (; argc; argc--) {
            CONS n; NEW(CONS, n, sizeof(struct CONS));
            n->a = getStr(argv[argc-1]);
            n->b = w;
            w = (LIST)n;
    }
    putStr(ROOT(w));
}
