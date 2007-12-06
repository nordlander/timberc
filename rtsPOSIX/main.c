#include "rts.h"
#include "timber.h"
#include "POSIX.h"

void ROOTINIT(void);
extern Prog_1_POSIX ROOT(Env_2_POSIX);

extern Prog_1_POSIX prog;
extern Env_2_POSIX env;

int main(int argc, char **argv) {
    init_rts(argc, argv);
    ROOTINIT();
    prog = ROOT(env);
    prog->start_5_POSIX(prog,-1,-1);
    run();
}
