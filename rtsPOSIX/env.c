#include <fcntl.h>
#include <pthread.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <errno.h>
#include "POSIX.h"
#include "env.h"

#define SIGSELECT SIGUSR1

#define ADD_RDTABLE(desc,act) {rdTable[desc] = act; FD_SET(desc,&readUsed); }
#define ADD_WRTABLE(desc,act) {wrTable[desc] = act; FD_SET(desc,&writeUsed); }

#define CLR_RDTABLE(desc) {rdTable[desc] = NULL; FD_CLR(desc, &readUsed);}
#define CLR_WRTABLE(desc) {wrTable[desc] = NULL; FD_CLR(desc, &writeUsed);}

// --------- Socket data stored in global array sockTable, indexed by descriptor -------------------------------------------------

struct SockData {
  WORD *GCINFO;
  struct sockaddr_in addr;              // address of remote peer
  SOCKHANDLER handler;      
};

WORD __GC__SockData[] = { WORDS(sizeof(struct SockData)), GC_STD, WORDS(offsetof(struct SockData,handler)), 0 };

typedef struct SockData *SockData;


// -------- Global variables ---------------------------------------------------

/*

  Bit n in readUsed is set iff
  - we have used installR to install a callback for the RFile with descriptor n; the callback is rdTable[n] OR
  - n is a (server) socket, we have called listen and are waiting for clients to connect; handler is sockTable[i] OR
  - n is a socket with an established connection; deliver method of the Destination is rdTable[i] and sockTable[i] contains
    SockData (needed for future closing message).

  Bit n in writeUsed is set iff
  - we have used installW to install a callback for the WFile with descriptor n; the callback is wrTable[i] OR
  - n is a (client) socket, we have called connect and are waiting for connection with server to be established; sockTable[i]
    contains SockData from which we can construct the handler when connection is set up.

  In all other cases, bits are cleared and array entries are NULL.

  eventLoop runs the indefinite loop that repetitively blocks on select; each change to the above data strucures is 
  reported to the calling thread through a SIGSELECT signal, so that select parameters can be adapted accordingly.

  maxDesc is an upper bound on the highest descriptor in use; updated when opening, but currently not decreased on closing.

*/


fd_set readUsed, writeUsed;

HANDLER rdTable[FD_SETSIZE] ;
ACTION  wrTable[FD_SETSIZE] ;
SockData sockTable[FD_SETSIZE];

int envRootsDirty;

struct Msg msg0 = { NULL, 0, { 0, 0 }, { INF, 0 }, NULL };

struct Thread thread0 = { NULL, &msg0, 0,  };

pthread_mutex_t envmut;

int maxDesc = 2;

ACTION prog                     = NULL;                         // Must be set by main()


//---------- Utilities ---------------------------------------------------------

Host_POSIX mkHost(struct sockaddr_in addr) {
  _Host_POSIX host; NEW(_Host_POSIX, host, WORDS(sizeof(struct _Host_POSIX)));
  host->GCINFO = __GC___Host_POSIX;
  host->a = getStr(inet_ntoa(addr.sin_addr));
  return (Host_POSIX)host;
}

Port_POSIX mkPort (struct sockaddr_in addr) {
  _Port_POSIX port; NEW(_Port_POSIX, port, WORDS(sizeof(struct _Port_POSIX)));
  port->GCINFO = __GC___Port_POSIX;
  port->a = ntohs(addr.sin_port); 
  return (Port_POSIX)port;
}

void netError(Int sock,char *message);

// -------- Closable -----------------------------------------------------------

struct DescClosable {
  WORD *GCINFO;
  UNITTYPE (*close_POSIX) (Closable_POSIX, Int);
//  Msg (*close_POSIX) (Closable_POSIX, Time, Time);
  int descriptor;
//  DescState self;
};
 
WORD __GC__DescClosable[] = { WORDS(sizeof(struct DescClosable)), GC_STD, 0 };

typedef struct DescClosable *DescClosable;

struct CloseMsg;
typedef struct CloseMsg *CloseMsg;

struct CloseMsg {
  WORD *GCINFO;
  UNITTYPE (*Code)(CloseMsg);
  AbsTime baseline;
  AbsTime deadline;
  Msg next;
  int descriptor;
//  DescState self;
};

WORD __GC__CloseMsg[] = { WORDS(sizeof(struct CloseMsg)), GC_STD, 0 };   // Field "next" is custom handled by the GC


UNITTYPE close_fun (Closable_POSIX this, Int dummy) {
  DISABLE(envmut);
  int desc = ((DescClosable)this)->descriptor;
  close(desc);
  CLR_RDTABLE(desc);
  CLR_WRTABLE(desc);
  sockTable[desc] = NULL;
  pthread_kill(thread0.id, SIGSELECT);
  ENABLE(envmut);
  return (UNITTYPE)0;
}

Closable_POSIX new_Closable (int desc) {
  DescClosable res;
  NEW(DescClosable, res, WORDS(sizeof(struct DescClosable)));
  res->GCINFO = __GC__DescClosable;
  res->close_POSIX = close_fun;
  res->descriptor = desc;
  return (Closable_POSIX)res;
}

// -------- File ---------------------------------------------------------------

Int seek_fun (File_POSIX this, Int off, Int dummy) {
  DISABLE(envmut);
  Int res, mode;
  if (off >= 0) 
    mode = SEEK_SET;
  else {
    mode = SEEK_END;
    off++;
  }
  res = lseek(((DescClosable)this->FILE2CLOSABLE)->descriptor,off,mode);
  ENABLE(envmut);
  return res;
}

File_POSIX new_File (int desc) {
  File_POSIX res; NEW(File_POSIX, res, WORDS(sizeof(struct File_POSIX)));
  res->GCINFO = __GC__File_POSIX;
  res->FILE2CLOSABLE = new_Closable(desc);
  res->seek_POSIX = seek_fun;
  return res;
}

// --------- RFile -------------------------------------------------------------

LIST read_descr (int descr) {
  char buf[1024];
  LIST xs = (LIST)0;
  LIST xslast = (LIST)0;
  LIST res = (LIST)0;
  LIST reslast = (LIST)0;
  int r;
  while (1) {
    xs = (LIST)0;
    xslast = (LIST)0;
    r = read(descr, buf, 1023);
    if (r <= 0) {
      if (reslast != (LIST)0) ((CONS)reslast)->b = (LIST)0;
      return res;
    }
    while (r) {
      CONS n; NEW(CONS, n, WORDS(sizeof(struct CONS)));
      if (xslast==(LIST)0) xslast = (LIST)n;
      n->GCINFO = __GC__CONS+5;                         // POLY instance is a scalar
      n->a = (POLY)(Int)buf[--r];
      n->b = xs;
      xs = (LIST)n;
    }
    if (res==(LIST)0) 
      res = xs; 
    else 
      ((CONS)reslast)->b = xs;
    reslast = xslast;
  }
}


LIST read_fun (RFile_POSIX this, Int dummy) {
  return read_descr(((DescClosable)this->RFILE2FILE->FILE2CLOSABLE)->descriptor);
}

// ----------- WFile -----------------------------------------------------------

int write_fun (WFile_POSIX this, LIST xs, Int dummy) {
  char buf[1024];
  int res = 0;
  while (xs) {
    int len = 0;
    while (xs && len < 1024) {
      buf[len++] = (Char)(Int)((CONS)xs)->a;
      xs = ((CONS)xs)->b;
    }
    if (len<1024) buf[len] = 0;
    int r = write(((DescClosable)this->WFILE2FILE->FILE2CLOSABLE)->descriptor, buf, len);
    if (r < 0) return res;
    res += r;
  }
  return res;
}

// ------------ Env ------------------------------------------------------------

UNITTYPE exit_fun (Env_POSIX this, Int n, Int dummy) {
  DISABLE(envmut);
  DISABLE(rts);
  exit(n);
}

Maybe_Prelude open_fun (LIST path, int oflag) {
  char buf[1024];
  int len = 0;
  while (path && len < 1024) {
    buf[len++] = (Char)(Int)((CONS)path)->a;
    path = ((CONS)path)->b;
  }
  buf[len] = 0;
  int descr = open(buf,oflag,S_IWUSR|S_IRUSR|S_IRGRP|S_IROTH);
  if (descr < 0) return (Maybe_Prelude)0;
  _Just_Prelude res; NEW(_Just_Prelude, res, WORDS(sizeof(struct _Just_Prelude)));
  res->GCINFO = __GC___Just_Prelude+0;           // POLY instance is a pointer
  res->a = (POLY)new_File(descr);
  return (Maybe_Prelude)res;
}

UNITTYPE installR_fun (RFile_POSIX this, HANDLER hand, Int dummy) {
  DISABLE(envmut);
  Int desc = ((DescClosable)this->RFILE2FILE->FILE2CLOSABLE)->descriptor;
  ADD_RDTABLE(desc,hand);
  maxDesc = desc > maxDesc ? desc : maxDesc;  
  pthread_kill(thread0.id, SIGSELECT);
  ENABLE(envmut);
  return (UNITTYPE)0;
}

UNITTYPE installW_fun (WFile_POSIX this, ACTION act, Int dummy) {
  DISABLE(envmut);
  Int desc = ((DescClosable)this->WFILE2FILE->FILE2CLOSABLE)->descriptor;
  ADD_WRTABLE(desc,act);
  envRootsDirty = 1;
  maxDesc = desc > maxDesc ? desc : maxDesc;  
  pthread_kill(thread0.id, SIGSELECT);
  ENABLE(envmut);
  return (UNITTYPE)0;
}

Maybe_Prelude openR_fun (Env_POSIX this, LIST path, Int dummy) {
  DISABLE(envmut);
  Maybe_Prelude f = open_fun(path,O_RDONLY);
  if (f) {
    RFile_POSIX rf; NEW(RFile_POSIX, rf, WORDS(sizeof(struct RFile_POSIX)));
    rf->GCINFO = __GC__RFile_POSIX;
    rf->RFILE2FILE = (File_POSIX)((_Just_Prelude)f)->a;
    rf->read_POSIX = read_fun;
    rf->installR_POSIX = installR_fun;
    ((_Just_Prelude)f)->a = (POLY)rf;
  }
  ENABLE(envmut);
  return f;
}

Maybe_Prelude openW_fun (Env_POSIX this, LIST path, Int dummy) {
  DISABLE(envmut);
  Maybe_Prelude f =  open_fun(path,O_WRONLY | O_CREAT | O_TRUNC);
  if (f) {
    WFile_POSIX wf; NEW(WFile_POSIX, wf, WORDS(sizeof(struct WFile_POSIX)));
    wf->GCINFO = __GC__WFile_POSIX;
    wf->WFILE2FILE = (File_POSIX)((_Just_Prelude)f)->a;
    wf->write_POSIX = write_fun;
    wf->installW_POSIX = installW_fun;
    ((_Just_Prelude)f)->a = (POLY)wf;
  }
  ENABLE(envmut);
  return f;
}

//---------- Time ---------------------------------------------------------------

Time getTime_fun (Env_POSIX this, Int dummy) {
  DISABLE(envmut);
  Time res; NEW(Time,res,WORDS(sizeof(struct Time)));
  res->GCINFO = __GC__Time;
  struct timeval now;
  if (gettimeofday(&now,NULL) < 0)
    perror("gettimeofday failed");
  res->sec = now.tv_sec;
  res->usec = now.tv_usec;
  ENABLE(envmut);
  return res;
}
  
//---------- Destination --------------------------------------------------------

struct DeliverMsg;
typedef struct DeliverMsg *DeliverMsg;

struct DeliverMsg {
  WORD *GCINFO;
  UNITTYPE (*Code)(DeliverMsg);
  AbsTime baseline;
  AbsTime deadline;
  Msg next;
  LIST str;
  int descriptor;
//  DescState self;
};

WORD __GC__DeliverMsg[] = { WORDS(sizeof(struct DeliverMsg)), GC_STD, WORDS(offsetof(struct DeliverMsg,str)), 0 };
                                                                      // Filed "next" is custom handled by the GC


UNITTYPE del_f (DeliverMsg this) {
//  DescState self = (DescState)LOCK((PID)this->self);
//  Int sock = self->desc;
//  UNLOCK((PID)self);
  int sock = this->descriptor;
  LIST str = this->str;
  char buf[1024];
  int len = 0;
  while (str && len < 1024) {
    buf[len++] = (Char)(Int)((CONS)str)->a;
    str = ((CONS)str)->b;
  }
  if (len < 1024) buf[len] = 0;
  if (write(sock,buf,len) < 0) {
    char errbuf[200];
    sprintf(errbuf,"Write failed: %s",strerror(errno));
    netError (sock,errbuf);
  }
  return (UNITTYPE)0;
}

Msg deliver_fun (Destination_POSIX this, LIST str, Time a, Time b) {
  DeliverMsg res;
  NEW(DeliverMsg,res,WORDS(sizeof(struct DeliverMsg)));
  res->GCINFO = __GC__DeliverMsg;
  res->Code = del_f;
  res->str = str;
  res->descriptor = ((DescClosable)this->DEST2CLOSABLE)->descriptor;
  ASYNC((Msg)res,a,b);
  return (Msg)res;
}

// ---------- Sockets ----------------------------------------------------------

Peer_POSIX new_Peer (Int sock) {
  Peer_POSIX res;
  Destination_POSIX dest;
  NEW (Destination_POSIX, dest, WORDS(sizeof(struct Destination_POSIX)));
  dest->GCINFO = __GC__Destination_POSIX;
  dest->DEST2CLOSABLE = new_Closable(sock);
  dest->deliver_POSIX = deliver_fun;
  NEW (Peer_POSIX, res, WORDS(sizeof(struct Peer_POSIX)));
  res->GCINFO = __GC__Peer_POSIX;
  res->PEER2DEST = dest;
  struct sockaddr_in addr = sockTable[sock]->addr;
  res->host_POSIX = mkHost(addr);
  res->port_POSIX = mkPort(addr);
  return res;
}

Int new_socket (SOCKHANDLER handler) {
  SockData d; 
  int sock = socket(PF_INET,SOCK_STREAM,0);
  fcntl(sock,F_SETFL,O_NONBLOCK);
  maxDesc = sock > maxDesc ? sock : maxDesc;  
  NEW(SockData,d,WORDS(sizeof(struct SockData)));
  d->GCINFO =__GC__SockData;
  d->handler = handler;
  sockTable[sock] = d;
  envRootsDirty = 1;
  return sock;
}  


struct HandlerStruct;
typedef struct HandlerStruct *HandlerStruct;
 
struct HandlerStruct {
  WORD *GCINFO;
  Msg (*Code) (HandlerStruct, LIST, Time, Time);
  Connection_POSIX conn;
};

WORD __GC__HandlerStruct[] = { WORDS(sizeof(struct HandlerStruct)), GC_STD, WORDS(offsetof(struct HandlerStruct, conn)), 0};

Msg handler_fun (HandlerStruct this, LIST str, Time a, Time b) {
  Destination_POSIX dest = this->conn->CONN2DEST;
  return dest->deliver_POSIX(dest,str,a,b);
}

HANDLER mkHandler (Connection_POSIX conn) {
  HandlerStruct rdHandler;
  NEW(HandlerStruct,rdHandler,WORDS(sizeof(HandlerStruct)));
  rdHandler->GCINFO = __GC__HANDLER;
  rdHandler->Code = handler_fun;
  rdHandler->conn = conn;
  return (HANDLER)rdHandler;
}

void netError (Int sock, char *message) {
  SOCKHANDLER handler = sockTable[sock]->handler;
  Connection_POSIX conn = (Connection_POSIX)handler->Code(handler,(POLY)new_Peer(sock),(POLY)0);
  ADD_RDTABLE(sock,mkHandler(conn));
  envRootsDirty = 1;
//  INTERRUPT_PROLOGUE();
  conn->neterror_POSIX(conn,getStr(message),Inherit,Inherit);
//  INTERRUPT_EPILOGUE();
}

void setupConnection (Int sock) {
  SOCKHANDLER handler = sockTable[sock]->handler;
  Connection_POSIX conn = (Connection_POSIX)handler->Code(handler,(POLY)new_Peer(sock),(POLY)0);
  ADD_RDTABLE(sock,mkHandler(conn));
  envRootsDirty = 1;
  conn->established_POSIX(conn,Inherit,Inherit);
}

int mkAddr (Int sock, Host_POSIX host, struct in_addr *addr) {
  LIST h = ((_Host_POSIX)host)->a;
  char buf[1024];
  int len = 0;
  Int hostid;
  struct hostent *ent;
  while (h && len < 1024) {
    buf[len++] = (Char)(Int)((CONS)h)->a;
    h = ((CONS)h)->b;
  }
  buf[len] = 0;
  // We assume gethostbyname will not block...
  ent = gethostbyname(buf);
  if(ent==NULL) {
    netError(sock,"Name lookup error");
    return -1;
  }
  else {
    memcpy(&hostid, ent->h_addr_list[0], sizeof hostid);
    addr->s_addr = hostid;
    return 0;
  }
}

UNITTYPE connect_fun (Sockets_POSIX this, Host_POSIX host, Port_POSIX port, SOCKHANDLER handler, Int dummy) {
  DISABLE(envmut);
  struct sockaddr_in addr;
  struct in_addr iaddr;
  int sock = new_socket(handler);
  if (mkAddr(sock, host, &iaddr) == 0) {
    addr.sin_addr = iaddr;
    addr.sin_port = htons(((_Port_POSIX)port)->a);
    addr.sin_family = AF_INET;
    sockTable[sock]->addr = addr;
    if (connect(sock,(struct sockaddr *)&addr,sizeof(struct sockaddr)) < 0) {// couldn't connect immediately, 
      if (errno==EINPROGRESS)                                                // so check if attempt continues asynchronously.
	FD_SET(sock,&writeUsed);
      else
	netError(sock,"Connect failed");
    }
    else {
      setupConnection(sock);
    }
    pthread_kill(thread0.id, SIGSELECT);
  }
  ENABLE(envmut);
  return (UNITTYPE)0;
}


Closable_POSIX listen_fun (Sockets_POSIX this, Port_POSIX port, SOCKHANDLER handler, Int dummy) {
  DISABLE(envmut);
  struct sockaddr_in addr;
  int sock = new_socket(handler);
  addr.sin_addr.s_addr = INADDR_ANY;
  addr.sin_port = htons(((_Port_POSIX)port)->a);
  addr.sin_family = AF_INET;
  if (bind(sock,(struct sockaddr *)&addr,sizeof(struct sockaddr)) < 0)
    perror("bind failed");
  listen(sock,5);
  FD_SET(sock,&readUsed);
  pthread_kill(thread0.id, SIGSELECT);
  ENABLE(envmut);
  return new_Closable(sock);
}

// ---------- Global env object ------------------------------------------------

// Note: all the following structs lie outside the garbage collected heap, and are therefore marked with a gcinfo = 0.

struct DescClosable stdin_cl    = { 0, close_fun, 0 };
struct DescClosable stdout_cl   = { 0, close_fun, 1 };

struct File_POSIX stdin_file    = { 0, (Closable_POSIX)&stdin_cl, seek_fun };
struct File_POSIX stdout_file   = { 0, (Closable_POSIX)&stdout_cl, seek_fun };

struct RFile_POSIX stdin_rfile  = { 0, &stdin_file, read_fun, installR_fun };
struct WFile_POSIX stdout_wfile = { 0, &stdout_file, write_fun, installW_fun };

struct Sockets_POSIX tcp        = { 0, connect_fun, listen_fun };

struct Internet_POSIX inet      = { 0, &tcp };

struct Env_POSIX env_struct     = { 0, exit_fun,  NULL, &stdin_rfile, &stdout_wfile,
                                    openR_fun, openW_fun, getTime_fun, &inet };

Env_POSIX env                   = &env_struct;


// ------- Copying for gc -----------------------------------------------

void scanEnvRoots (void) {
        envRootsDirty = 0;
        int i;
        prog = (ACTION)copy((ADDR)prog);
        i = 0;
        DISABLE(envmut);
        while (i<maxDesc+1) {
                if (rdTable[i]) rdTable[i] = (HANDLER)copy((ADDR)rdTable[i]);
                if (wrTable[i]) wrTable[i] = (ACTION)copy((ADDR)wrTable[i]);
                if (sockTable[i]) sockTable[i] = (SockData)copy((ADDR)sockTable[i]);
                i++;
                ENABLE(envmut);
                DISABLE(envmut);
        }
        ENABLE(envmut);
}

// --------- Event loop ----------------------------------------------

void kill_handler () {
    return;
}

void eventLoop (void) {
    sigset_t one_sig;
    sigemptyset(&one_sig);
    sigaddset(&one_sig, SIGSELECT);
    pthread_sigmask(SIG_UNBLOCK, &one_sig, NULL);

    DISABLE(envmut);
    fd_set readFds, writeFds;
    int i;
    while(1) {
        FD_COPY(&readUsed, &readFds);
        FD_COPY(&writeUsed, &writeFds);
        ENABLE(envmut);
        int r = select(maxDesc+1, &readFds, &writeFds, NULL, NULL);
        DISABLE(envmut);
        if (r >= 0) {
            TIMERGET(msg0.baseline);
            for(i=0; i<maxDesc+1; i++) {
	            if (FD_ISSET(i, &readFds)) {
	                if (rdTable[i]) {
	                    LIST inp = read_descr(i);
	                    if (inp) {
	                        rdTable[i]->Code(rdTable[i],(POLY)inp,(POLY)Inherit,(POLY)Inherit);
	                    }
	                    else if (sockTable[i]) { //we got a close message from peer on connected socket
	                        SOCKHANDLER handler = sockTable[i]->handler;
	                        Connection_POSIX conn = (Connection_POSIX)handler->Code(handler,(POLY)new_Peer(i),(POLY)0);
                            Closable_POSIX cl = conn->CONN2DEST->DEST2CLOSABLE;
	                        cl->close_POSIX(cl,0);
	                        close(i);
	                        CLR_RDTABLE(i);
	                        sockTable[i] = NULL;
	                    }
	                } else if (sockTable[i]) { //listening socket received a connect request; we will accept
	                    socklen_t len = sizeof(struct sockaddr);
	                    struct sockaddr_in addr;
	                    Int sock = accept(i,(struct sockaddr *)&addr,&len);
	                    fcntl(sock,F_SETFL,O_NONBLOCK);
	                    NEW(SockData,sockTable[sock],WORDS(sizeof(struct SockData)));
	                    sockTable[sock]->handler = sockTable[i]->handler;
	                    sockTable[sock]->addr = addr;
	                    maxDesc = sock > maxDesc ? sock : maxDesc;
	                    setupConnection(sock);
	                }
	            }
	            if (FD_ISSET(i, &writeFds)) {
	                if (wrTable[i]) {
	                    wrTable[i]->Code(wrTable[i],(POLY)Inherit,(POLY)Inherit);
	                } else if (sockTable[i]) { //delayed connection has been accepted or has failed
	                    int opt;
	                    socklen_t len = sizeof(int);
	                    FD_CLR(i,&writeUsed);
	                    if (getsockopt(i,SOL_SOCKET,SO_ERROR, (void *)&opt, &len) < 0)
	                        perror("getsockopt failed");
	                    if (opt) {
	                        netError(i,"Connection failed");
	                    } else {
	                        setupConnection(i);
	                    }
	                }
	            }
            }
        }
    }
}

// --------- Initialization ----------------------------------------------------

void envInit (int argc, char **argv) {
    Int i;
  
    pthread_setspecific(current_key, &thread0);
    pthread_mutex_init(&envmut, &glob_mutexattr);
  
    FD_ZERO(&readUsed);
    FD_ZERO(&writeUsed);
  
    struct sigaction act;
    act.sa_flags = 0;
    sigemptyset( &act.sa_mask );
    act.sa_handler = kill_handler;
    sigaction( SIGSELECT, &act, NULL );
  
    Array arr; NEW(Array,arr,WORDS(sizeof(struct Array))+argc);
    arr->GCINFO = __GC__Array0;
    arr->size = argc;
    for (i=0; i<argc; i++)
        arr->elems[i] = (POLY)getStr(argv[i]);
    env->argv_POSIX = arr;
  
    fcntl(0, F_SETFL, O_NONBLOCK);
    fcntl(1, F_SETFL, O_NONBLOCK);

    TIMERGET(msg0.baseline);

    thread0.msg = &msg0;
    thread0.id = pthread_self();
  }
