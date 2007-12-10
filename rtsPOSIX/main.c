#include "rts.h"
#include "timber.h"
#include "POSIX.h"

void ROOTINIT(void);
extern Prog_POSIX ROOT(Env_POSIX);

extern Prog_POSIX prog;
extern Env_POSIX env;

int main(int argc, char **argv) {
    init_rts(argc, argv);
    ROOTINIT();
    prog = ROOT(env);
    prog->start_POSIX(prog,-1,-1);
    run();
}
