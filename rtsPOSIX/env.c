#include <fcntl.h>
#include "POSIX.h"


struct DescFile {
        WORD *gcinfo;
        LIST (*read_POSIX) (File_POSIX, POLY);
        LIST (*write_POSIX) (File_POSIX, LIST, POLY);
        int desc;
};
typedef struct DescFile *DescFile;
WORD __GC__DescFile[] = { WORDS(sizeof(struct DescFile)), 0 };

LIST read_fun( File_POSIX this, POLY self ) {
        sigset_t previous_mask;
        char buf[1024];
        LIST xs = (LIST)_NIL;
        LIST xslast = (LIST)_NIL;
        LIST res = (LIST)_NIL;
        LIST reslast = (LIST)_NIL;
        while (1) {
                xs = (LIST)_NIL;
                xslast = (LIST)_NIL;
                DISABLE(&previous_mask);
                int r = read(((DescFile)this)->desc, buf, 1023);
                ENABLE(&previous_mask);
                if (r <= 0) {
		        if (reslast != (LIST)_NIL) ((CONS)reslast)->b = (LIST)_NIL;
                        return res;
	        }
                while (r) {
                        CONS n; NEW(CONS, n, sizeof(struct CONS));
                        if (xslast==(LIST)_NIL) xslast = (LIST)n;
                        SETGCINFO(n, __GC__CONS);
                        n->a = (POLY)(Int)buf[--r];
                        n->b = xs;
                        xs = (LIST)n;
                }
                if (res==(LIST)_NIL) 
		        res = xs; 
                else 
                        ((CONS)reslast)->b = xs;
                reslast = xslast;
        }
}

LIST write_fun( File_POSIX this, LIST xs, POLY self ) {
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

UNITTYPE exit_fun( Env_POSIX this, Int n, POLY self ) {
        DISABLE(NULL);
        exit(n);
}

File_POSIX open_fun(Env_POSIX this, LIST path, AccessMode_POSIX mode, POLY self) {
        sigset_t previous_mask;
        char buf[1024];
        int oflag = (mode==_Read_POSIX ? O_RDONLY : O_WRONLY) | O_CREAT;
        int len = 0;
        while (path && len < 1024) {
                 buf[len++] = (Char)(Int)((CONS)path)->a;
                 path = ((CONS)path)->b;
                }
        DISABLE(&previous_mask);
        int descr = open(buf,oflag,S_IWUSR|S_IRUSR|S_IRGRP|S_IROTH);
        ENABLE(&previous_mask);
        DescFile file_struct; NEW(DescFile, file_struct, sizeof(struct DescFile));
        SETGCINFO(file_struct,__GC__DescFile);
        file_struct->read_POSIX = read_fun;
        file_struct->write_POSIX = write_fun;
        file_struct->desc = descr;
        return (File_POSIX) file_struct;
}


struct DescFile stdin_struct    = { __GC__DescFile, read_fun, write_fun , 0 };

struct DescFile stdout_struct   = { __GC__DescFile, read_fun, write_fun, 1 };

struct Env_POSIX env_struct   = { __GC__Env_POSIX, NULL, (File_POSIX)&stdin_struct, (File_POSIX)&stdout_struct, exit_fun, open_fun };

Env_POSIX env                 = &env_struct;

Prog_POSIX prog               = NULL;                         // Must be set by main()


void copyEnvRoots(void) {
        prog = (Prog_POSIX)copy((ADDR)prog);
}

void io_handler(int signo) {
        INTERRUPT_PROLOGUE();
        prog->io_POSIX(prog, -1, -1);
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
                SETGCINFO(n, __GC__CONS);
                n->a = getStr(argv[argc-1]);
                n->b = w;
                w = (LIST)n;
        }
        env->argv_POSIX = w;

        fcntl(0, F_SETFL, O_NONBLOCK + O_ASYNC);
        fcntl(1, F_SETFL, O_NONBLOCK + O_ASYNC);
}
