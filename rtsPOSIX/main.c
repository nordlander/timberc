#include "rts.h"
#include "timber.h"
#include "POSIX.h"
#include "env.h"

void ROOTINIT(void);
ACTION ROOT(Env_POSIX, POLY);

extern ACTION prog;
extern Env_POSIX env;

int main(int argc, char **argv) {
    init_rts(argc, argv);
    ROOTINIT();
    pruneStaticHeap();
    prog = ROOT(env, (POLY)0);
    prog->Code(prog,(POLY)Inherit,(POLY)Inherit);
    eventLoop();
}
