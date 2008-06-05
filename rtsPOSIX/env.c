#include <fcntl.h>
#include <pthread.h>
#include <unistd.h>
#include "POSIX.h"

#define ADD_RDTABLE(desc,act) {rdTable[desc] = act;}
#define ADD_WRTABLE(desc,act) {wrTable[desc] = act;}

struct DescFile {
    WORD *gcinfo;
    UNITTYPE (*close_POSIX) (File_POSIX, POLY);
    Int (*seek_POSIX) (File_POSIX, Int, POLY);
    int desc;
};
 
typedef struct DescFile *DescFile;
fd_set readUsed, writeUsed;


WORD __GC__DescFile[] = { WORDS(sizeof(struct DescFile)), 0 };

ACTION rdTable[FD_SETSIZE] , wrTable[FD_SETSIZE] ;

int maxDesc = 2;

LIST read_fun( RFile_POSIX this, POLY self ) {
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
                int r = read(((DescFile)this->l_119_POSIX)->desc, buf, 1023);
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

int write_fun( WFile_POSIX this, LIST xs, POLY self ) {
        sigset_t previous_mask;
        char buf[1024];
        int res = 0;
        while (xs) {
                int len = 0;
                while (xs && len < 1024) {
                        buf[len++] = (Char)(Int)((CONS)xs)->a;
                        xs = ((CONS)xs)->b;
                }
                DISABLE(&previous_mask);
                int r = write(((DescFile)this->l_122_POSIX)->desc, buf, len);
                ENABLE(&previous_mask);
                if (r < 0) return res;
                res += r;
        }
        return res;
}

UNITTYPE exit_fun( Env_POSIX this, Int n, POLY self ) {
        DISABLE(NULL);
        exit(n);
}

UNITTYPE close_fun(File_POSIX this, POLY self) {
        sigset_t previous_mask;
        DISABLE(&previous_mask);
        close(((DescFile)this)->desc);
        ENABLE(&previous_mask);
        rdTable[((DescFile)this)->desc] = NULL;
        wrTable[((DescFile)this)->desc] = NULL;
        return _UNITTERM;
}

Int seek_fun(File_POSIX this, Int off, POLY self) {
        sigset_t previous_mask;
        Int res, mode;
	if (off >= 0) 
	    mode = SEEK_SET;
	else {
	    mode = SEEK_END;
	    off++;
        }
        DISABLE(&previous_mask);
	res = lseek(((DescFile)this)->desc,off,mode);
        ENABLE(&previous_mask);
	return res;
}

File_POSIX new_file(int desc) {
        DescFile res; NEW(DescFile, res, sizeof(struct DescFile));
        SETGCINFO(res,__GC__DescFile);
        res->close_POSIX = close_fun;
        res->seek_POSIX = seek_fun;
        res->desc = desc;
        return (File_POSIX)res;
}

Maybe_Prelude open_fun(Env_POSIX this, LIST path, POLY self, int oflag) {
        sigset_t previous_mask;
        char buf[1024];
        int len = 0;
        while (path && len < 1024) {
            buf[len++] = (Char)(Int)((CONS)path)->a;
            path = ((CONS)path)->b;
        }
        DISABLE(&previous_mask);
        int descr = open(buf,oflag,S_IWUSR|S_IRUSR|S_IRGRP|S_IROTH);
        ENABLE(&previous_mask);
        if (descr < 0) return NULL;
        maxDesc = descr > maxDesc ? descr : maxDesc;
        Just_Prelude res; NEW(Just_Prelude, res, sizeof(struct Just_Prelude));
	SETGCINFO(res,__GC__Just_Prelude);
        res->a = (POLY)new_file(descr);
	return (Maybe_Prelude)res;
}

Maybe_Prelude openR_fun(Env_POSIX this, LIST path, POLY self) {
  Maybe_Prelude f = open_fun(this,path,self,O_RDONLY);
  if (f) {
        RFile_POSIX rf; NEW(RFile_POSIX, rf, sizeof(struct RFile_POSIX));
        SETGCINFO(rf,__GC__RFile_POSIX);
        rf->l_119_POSIX = (File_POSIX)((Just_Prelude)f)->a;
        rf->read_POSIX = read_fun;
        ((Just_Prelude)f)->a = (POLY)rf;
        FD_SET(((DescFile)((RFile_POSIX)((Just_Prelude)f)->a)->l_119_POSIX)->desc, &readUsed);
  }
  return f;
}

Maybe_Prelude openW_fun(Env_POSIX this, LIST path, POLY self) {
  Maybe_Prelude f =  open_fun(this,path,self,O_WRONLY | O_CREAT);
  if (f) {
        WFile_POSIX wf; NEW(WFile_POSIX, wf, sizeof(struct WFile_POSIX));
        SETGCINFO(wf,__GC__WFile_POSIX);
        wf->l_122_POSIX = (File_POSIX)((Just_Prelude)f)->a;
        wf->write_POSIX = write_fun;
        ((Just_Prelude)f)->a = (POLY)wf;
        FD_SET(((DescFile)((WFile_POSIX)((Just_Prelude)f)->a)->l_122_POSIX)->desc, &writeUsed);
  }
  return f;
}

UNITTYPE installR_fun (Env_POSIX this, RFile_POSIX rf, ACTION act, POLY self) {
  ADD_RDTABLE(((DescFile)rf->l_119_POSIX)->desc,act);
  return _UNITTERM;
}

UNITTYPE installW_fun (Env_POSIX this, WFile_POSIX wf, ACTION act, POLY self) {
  ADD_WRTABLE(((DescFile)wf->l_122_POSIX)->desc,act);
  return _UNITTERM;
}

struct DescFile stdin_file      = { __GC__DescFile, close_fun, seek_fun, STDIN_FILENO };
struct DescFile stdout_file     = { __GC__DescFile, close_fun, seek_fun, STDOUT_FILENO };

struct RFile_POSIX stdin_rfile  = { __GC__RFile_POSIX, (File_POSIX)&stdin_file, read_fun };
struct WFile_POSIX stdout_wfile = { __GC__WFile_POSIX, (File_POSIX)&stdout_file, write_fun };

struct Env_POSIX env_struct     = { __GC__Env_POSIX, exit_fun,  NULL, (RFile_POSIX)&stdin_rfile, (WFile_POSIX)&stdout_wfile,
                                    openR_fun, openW_fun, installR_fun, installW_fun };

Env_POSIX env                   = &env_struct;

ACTION prog                     = NULL;                         // Must be set by main()


void copyEnvRoots(void) {
     int i;
     prog = (ACTION)copy((ADDR)prog);
     for(i = 0; i<maxDesc+1; i++) {
         if (rdTable[i]) rdTable[i] = (ACTION)copy((ADDR)rdTable[i]);
         if (wrTable[i]) wrTable[i] = (ACTION)copy((ADDR)wrTable[i]);
     }
	
}

/*
void io_handler() {
        INTERRUPT_PROLOGUE();
        prog->Code(prog, -1, -1);
        INTERRUPT_EPILOGUE();
}
*/

void *event_loop(void *arg) {
   fd_set readFds, writeFds;
   int i;
   while(1) {
        FD_COPY(&readUsed, &readFds);
        FD_COPY(&writeUsed, &writeFds);
        select(maxDesc+1, &readFds, &writeFds, NULL, NULL);
        for(i=0; i<maxDesc+1; i++) {
            if (FD_ISSET(i, &readFds)) {
	        if (rdTable[i]) {
	            INTERRUPT_PROLOGUE();
	            rdTable[i]->Code(rdTable[i],-1,-1);
                    INTERRUPT_EPILOGUE();
		}
	        if (wrTable[i]) {
	            INTERRUPT_PROLOGUE();
	            wrTable[i]->Code(wrTable[i],-1,-1);
                    INTERRUPT_EPILOGUE();
		}
	    }
	}
   }
}

  void init_env(int argc, char **argv) {
        pthread_t evThread;
        FD_ZERO(&readUsed);
        FD_ZERO(&writeUsed);
        FD_SET(0,&readUsed);
        FD_SET(1,&writeUsed);
        pthread_create(&evThread, NULL, event_loop, NULL); 
	/*
        struct sigaction act;
        act.sa_flags = 0;
        sigemptyset( &act.sa_mask );
        act.sa_handler = io_handler;
        sigaction( SIGIO, &act, NULL );
	*/
        LIST w = (LIST)_NIL;
        for (; argc; argc--) {
                CONS n; NEW(CONS, n, sizeof(struct CONS));
                SETGCINFO(n, __GC__CONS);
                n->a = getStr(argv[argc-1]);
                n->b = w;
                w = (LIST)n;
        }
        env->argv_POSIX = w;

        fcntl(0, F_SETFL, O_NDELAY);
	fcntl(1, F_SETFL, O_NDELAY);
  }
