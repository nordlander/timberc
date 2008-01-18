#include "rts.h"
#include "timber.h"
#include "POSIX.h"

void ROOTINIT(void);
Prog_POSIX ROOT(Env_POSIX, POLY);

extern Prog_POSIX prog;
extern Env_POSIX env;

int main(int argc, char **argv) {
    init_rts(argc, argv);
    ROOTINIT();
    pruneStaticHeap();
    prog = ROOT(env, (POLY)0);
    prog->start_POSIX(prog,-1,-1);
    run();
}
