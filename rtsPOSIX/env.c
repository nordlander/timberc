#include <fcntl.h>
#include "POSIX.h"



struct DescFile {
        WORD *gcinfo;
        LIST (*read_11_POSIX) (File_3_POSIX, POLY);
        LIST (*write_12_POSIX) (File_3_POSIX, LIST, POLY);
        int desc;
};
typedef struct DescFile *DescFile;
WORD __GC__DescFile[] = { sizeof(struct DescFile), 0 };

LIST read_fun( File_3_POSIX this, POLY self ) {
        sigset_t previous_mask;
        char buf[1024];
        LIST xs = (LIST)_NIL;
        while (1) {
                DISABLE(&previous_mask);
                int r = read(((DescFile)this)->desc, buf, 1023);
                ENABLE(&previous_mask);
                if (r <= 0)
                        return xs;
                while (r) {
                        CONS n; NEW(CONS, n, sizeof(struct CONS));
                        n->a = (POLY)(Int)buf[--r];
                        n->b = xs;
                        xs = (LIST)n;
                }
        }
}

LIST write_fun( File_3_POSIX this, LIST xs, POLY self ) {
        sigset_t previous_mask;
        char buf[1024];
        while (xs) {
                LIST xs0 = xs;
                int len = 0;
                while (xs && len < 1024) {
                        buf[len++] = (Char)(Int)((CONS)xs)->a;
                        xs = ((CONS)xs)->b;
                }
                DISABLE(&previous_mask);
                int r = write(((DescFile)this)->desc, buf, len);
                ENABLE(&previous_mask);
                if (r < 0) r = 0;
                if (r < len) {
                        while (r--)
                                xs0 = ((CONS)xs0)->b;
                        return xs0;
                }
        }
        return (LIST)_NIL;
}

UNITTYPE exit_fun( Env_2_POSIX this, Int n, POLY self ) {
        DISABLE(NULL);
        exit(n);
}

struct DescFile stdin_struct    = { __GC__DescFile, read_fun, write_fun , 0 };

struct DescFile stdout_struct   = { __GC__DescFile, read_fun, write_fun, 1 };

struct Env_2_POSIX env_struct   = { __GC__Env_2_POSIX, NULL, (File_3_POSIX)&stdin_struct, (File_3_POSIX)&stdout_struct, exit_fun };

Env_2_POSIX env                 = &env_struct;

Prog_1_POSIX prog               = NULL;                         // Must be set by main()


int copyEnvRoots() {        
        prog = (Prog_1_POSIX)copy((ADDR)prog);
        env->argv_7_POSIX = (LIST)copy((ADDR)env->argv_7_POSIX);
}

void io_handler(int signo) {
        INTERRUPT_PROLOGUE();
        prog->io_6_POSIX(prog, -1, -1);
        INTERRUPT_EPILOGUE();
}


void init_env(int argc, char **argv) {
        struct sigaction act;
        act.sa_flags = 0;
        sigemptyset( &act.sa_mask );
        act.sa_handler = io_handler;
        sigaction( SIGIO, &act, NULL );

        LIST w = (LIST)_NIL;
        for (; argc; argc--) {
                CONS n; NEW(CONS, n, sizeof(struct CONS));
                n->a = getStr(argv[argc-1]);
                n->b = w;
                w = (LIST)n;
        }
        env->argv_7_POSIX = w;

        fcntl(0, F_SETFL, O_NONBLOCK + O_ASYNC);
        fcntl(1, F_SETFL, O_NONBLOCK + O_ASYNC);
}
