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
  WORD *gcinfo;
  struct sockaddr_in addr;              // address of remote peer
  SOCKHANDLER handler;      
};

WORD __GC__SockData[] = { WORDS(sizeof(struct SockData)), WORDS(offsetof(struct SockData,handler)), 0 };

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

  evThread runs the selectloop; each change to the above data strucures is reported to the eventloop thread through a
  SIGSELECT signal, so that select parameters can be adapted accordingly.

  maxDesc is an upper bound on the highest descriptor in use; updated when opening, but currently not decreased on closing.

*/


fd_set readUsed, writeUsed;

HANDLER rdTable[FD_SETSIZE] ;
ACTION  wrTable[FD_SETSIZE] ;
SockData sockTable[FD_SETSIZE];

pthread_t evThread;

int maxDesc = 2;

ACTION prog                     = NULL;                         // Must be set by main()

//---------- Utilities ---------------------------------------------------------

Host_POSIX mkHost(struct sockaddr_in addr) {
  Host_POSIX host; NEW(Host_POSIX, host, sizeof(struct Host_POSIX));
  SETGCINFO(host,__GC__Host_POSIX);
  host->Tag = (Int)getStr(inet_ntoa(addr.sin_addr));
  return host;
}

Port_POSIX mkPort (struct sockaddr_in addr) {
  Port_POSIX port; NEW(Port_POSIX, port, sizeof(struct Port_POSIX));
  SETGCINFO(port,__GC__Port_POSIX);
  port->Tag = ntohs(addr.sin_port); 
  return port;
}

void netError(Int sock,char *message);

// --------- State object storing file desciptor ------------------------------- 

struct DescState {
  WORD *gcinfo;
  Thread ownedBy;
  Thread wantedBy;
  Int desc;
};

WORD __GC__DescState[] = { WORDS(sizeof(struct DescState)),0 };

typedef struct DescState *DescState;


DescState new_DescState (int desc) {
  DescState res;
  NEW (DescState, res, WORDS(sizeof(struct DescState)));
  SETGCINFO(res, __GC__DescState);
  res->ownedBy = (Thread)0;
  res->wantedBy = (Thread)0;
  res->desc = desc;
  return res;
}



// -------- Closable -----------------------------------------------------------

struct DescClosable {
  WORD *gcinfo;
  Msg (*close_POSIX) (Closable_POSIX, Time, Time);
  DescState self;
};
 
WORD __GC__DescClosable[] = { WORDS(sizeof(struct DescClosable)), WORDS(offsetof(struct DescClosable,self)),0 };

typedef struct DescClosable *DescClosable;

struct CloseMsg;
typedef struct CloseMsg *CloseMsg;

struct CloseMsg {
  WORD *gcinfo;
  UNITTYPE (*Code)(CloseMsg);
  AbsTime baseline;
  AbsTime deadline;
  Msg next;
  DescState self;
};

WORD __GC__CloseMsg[] = { WORDS(sizeof(struct CloseMsg)), WORDS(offsetof(struct CloseMsg,next)),
			  WORDS(offsetof(struct CloseMsg,self)), 0 };


UNITTYPE cl_fun (CloseMsg this) {
  DescState self = (DescState)LOCK((PID)this->self);
  Int desc = self->desc;
  UNLOCK((PID)self);
  close(desc);
  CLR_RDTABLE(desc);
  CLR_WRTABLE(desc);
  sockTable[desc] = NULL;
  pthread_kill(evThread,SIGSELECT);
  return _UNITTERM;
}

Msg close_fun (Closable_POSIX this, Time a, Time b) {
  CloseMsg res;
  NEW(CloseMsg,res,WORDS(sizeof(struct CloseMsg)));
  SETGCINFO(res,__GC__CloseMsg);
  res->Code  = cl_fun;
  res->self = ((DescClosable)this)->self;
  ASYNC((Msg)res,a,b);
  return (Msg)res;
}

Closable_POSIX new_Closable (int desc) {
  DescClosable res;
  NEW(DescClosable, res, sizeof(struct DescClosable));
  SETGCINFO(res,__GC__DescClosable);
  res->close_POSIX = close_fun;
  res->self = new_DescState(desc);
  return (Closable_POSIX)res;
}

// -------- File ---------------------------------------------------------------

Int seek_fun (File_POSIX this, Int off, POLY self) {
  Int res, mode;
  if (off >= 0) 
    mode = SEEK_SET;
  else {
    mode = SEEK_END;
    off++;
  }
  res = lseek(((DescClosable)this->FILE2CLOSABLE)->self->desc,off,mode);
  return res;
}

File_POSIX new_File (int desc) {
  File_POSIX res; NEW(File_POSIX, res, sizeof(struct File_POSIX));
  SETGCINFO(res,__GC__File_POSIX);
  res->FILE2CLOSABLE = new_Closable(desc);
  res->seek_POSIX = seek_fun;
  return res;
}

// --------- RFile -------------------------------------------------------------

LIST read_descr (int descr) {
  char buf[1024];
  LIST xs = (LIST)_NIL;
  LIST xslast = (LIST)_NIL;
  LIST res = (LIST)_NIL;
  LIST reslast = (LIST)_NIL;
  int r;
  while (1) {
    xs = (LIST)_NIL;
    xslast = (LIST)_NIL;
    r = read(descr, buf, 1023);
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


LIST read_fun (RFile_POSIX this, POLY self) {
  return read_descr(((DescClosable)this->RFILE2FILE->FILE2CLOSABLE)->self->desc);
}

// ----------- WFile -----------------------------------------------------------

int write_fun (WFile_POSIX this, LIST xs, POLY self) {
  char buf[1024];
  int res = 0;
  while (xs) {
    int len = 0;
    while (xs && len < 1024) {
      buf[len++] = (Char)(Int)((CONS)xs)->a;
      xs = ((CONS)xs)->b;
    }
    if (len<1024) buf[len] = 0;
    int r = write(((DescClosable)this->WFILE2FILE->FILE2CLOSABLE)->self->desc, buf, len);
    if (r < 0) return res;
    res += r;
  }
  return res;
}

// ------------ Env ------------------------------------------------------------

UNITTYPE exit_fun (Env_POSIX this, Int n, POLY self) {
  DISABLE(NULL);
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
  if (descr < 0) return NULL;
  Just_Prelude res; NEW(Just_Prelude, res, sizeof(struct Just_Prelude));
  SETGCINFO(res,__GC__Just_Prelude);
  res->a = (POLY)new_File(descr);
  return (Maybe_Prelude)res;
}

UNITTYPE installR_fun (RFile_POSIX this, HANDLER hand, POLY self) {
  Int desc = ((DescClosable)this->RFILE2FILE->FILE2CLOSABLE)->self->desc;
  ADD_RDTABLE(desc,hand);
  maxDesc = desc > maxDesc ? desc : maxDesc;  
  pthread_kill(evThread,SIGSELECT);
  return _UNITTERM;
}

UNITTYPE installW_fun (WFile_POSIX this, ACTION act, POLY self) {
  Int desc = ((DescClosable)this->WFILE2FILE->FILE2CLOSABLE)->self->desc;
  ADD_WRTABLE(desc,act);
  maxDesc = desc > maxDesc ? desc : maxDesc;  
  pthread_kill(evThread,SIGSELECT);
  return _UNITTERM;
}

Maybe_Prelude openR_fun (Env_POSIX this, LIST path, POLY self) {
  Maybe_Prelude f = open_fun(path,O_RDONLY);
  if (f) {
    RFile_POSIX rf; NEW(RFile_POSIX, rf, sizeof(struct RFile_POSIX));
    SETGCINFO(rf,__GC__RFile_POSIX);
    rf->RFILE2FILE = (File_POSIX)((Just_Prelude)f)->a;
    rf->read_POSIX = read_fun;
    rf->installR_POSIX = installR_fun;
    ((Just_Prelude)f)->a = (POLY)rf;
  }
  return f;
}

Maybe_Prelude openW_fun (Env_POSIX this, LIST path, POLY self) {
  Maybe_Prelude f =  open_fun(path,O_WRONLY | O_CREAT);
  if (f) {
    WFile_POSIX wf; NEW(WFile_POSIX, wf, sizeof(struct WFile_POSIX));
    SETGCINFO(wf,__GC__WFile_POSIX);
    wf->WFILE2FILE = (File_POSIX)((Just_Prelude)f)->a;
    wf->write_POSIX = write_fun;
    wf->installW_POSIX = installW_fun;
    ((Just_Prelude)f)->a = (POLY)wf;
  }
  return f;
}


//---------- Destination --------------------------------------------------------

struct DeliverMsg;
typedef struct DeliverMsg *DeliverMsg;

struct DeliverMsg {
  WORD *gcinfo;
  UNITTYPE (*Code)(DeliverMsg);
  AbsTime baseline;
  AbsTime deadline;
  Msg next;
  LIST str;
  DescState self;
};

WORD __GC__DeliverMsg[] = { WORDS(sizeof(struct DeliverMsg)), WORDS(offsetof(struct DeliverMsg,next)),
		            WORDS(offsetof(struct DeliverMsg,str)), WORDS(offsetof(struct DeliverMsg,self)),0 };


UNITTYPE del_f (DeliverMsg this) {
  DescState self = (DescState)LOCK((PID)this->self);
  Int sock = self->desc;
  UNLOCK((PID)self);
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
  return _UNITTERM;
}

Msg deliver_fun (Destination_POSIX this, LIST str, Time a, Time b) {
  DeliverMsg res;
  NEW(DeliverMsg,res,WORDS(sizeof(struct DeliverMsg)));
  SETGCINFO(res,__GC__DeliverMsg);
  res->Code = del_f;
  res->str = str;
  res->self = ((DescClosable)this->DEST2CLOSABLE)->self;
  ASYNC((Msg)res,a,b);
  return (Msg)res;
}

// ---------- Sockets ----------------------------------------------------------

Peer_POSIX new_Peer (Int sock) {
  Peer_POSIX res;
  Destination_POSIX dest;
  NEW (Destination_POSIX, dest, WORDS(sizeof(struct Destination_POSIX)));
  SETGCINFO(dest, __GC__Destination_POSIX);
  dest->DEST2CLOSABLE = new_Closable(sock);
  dest->deliver_POSIX = deliver_fun;
  NEW (Peer_POSIX, res, WORDS(sizeof(struct Peer_POSIX)));
  SETGCINFO(res, __GC__Peer_POSIX);
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
  SETGCINFO(d,__GC__SockData);
  d->handler = handler;
  sockTable[sock] = d;
  return sock;
}  


struct HandlerStruct;
typedef struct HandlerStruct *HandlerStruct;
 
struct HandlerStruct {
  WORD *gcinfo;
  Msg (*Code) (HandlerStruct, LIST, Time, Time);
  Connection_POSIX conn;
};

WORD __GC__HandlerStruct[] = { WORDS(sizeof(struct HandlerStruct)), WORDS(offsetof(struct HandlerStruct, conn)), 0};

Msg handler_fun (HandlerStruct this, LIST str, Time a, Time b) {
  Destination_POSIX dest = this->conn->CONN2DEST;
  return dest->deliver_POSIX(dest,str,a,b);
}

HANDLER mkHandler (Connection_POSIX conn) {
  HandlerStruct rdHandler;
  NEW(HandlerStruct,rdHandler,sizeof(HandlerStruct));
  SETGCINFO(rdHandler,__GC__HANDLER);
  rdHandler->Code = handler_fun;
  rdHandler->conn = conn;
  return (HANDLER)rdHandler;
}

void netError (Int sock, char *message) {
  SOCKHANDLER handler = sockTable[sock]->handler;
  Connection_POSIX conn = handler->Code(handler,new_Peer(sock),NULL);
  ADD_RDTABLE(sock,mkHandler(conn));
  INTERRUPT_PROLOGUE();
  conn->neterror_POSIX(conn,getStr(message),Inherit,Inherit);
  INTERRUPT_EPILOGUE();
}

void setupConnection (Int sock) {
  SOCKHANDLER handler = sockTable[sock]->handler;
  Connection_POSIX conn = handler->Code(handler,new_Peer(sock),NULL);
  ADD_RDTABLE(sock,mkHandler(conn));
  INTERRUPT_PROLOGUE();
  conn->established_POSIX(conn,Inherit,Inherit);
  INTERRUPT_EPILOGUE();
}

int mkAddr (Int sock, Host_POSIX host, struct in_addr *addr) {
  LIST h = (LIST)host->Tag;
  // Hack: we use Tag to avoid needing to know name generated in POSIX.h of the struct with the LIST
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

UNITTYPE connect_fun (Sockets_POSIX this, Host_POSIX host, Port_POSIX port, SOCKHANDLER handler, POLY self) {
  struct sockaddr_in addr;
  struct in_addr iaddr;
  int sock = new_socket(handler);
  if (mkAddr(sock, host, &iaddr) == 0) {
    addr.sin_addr = iaddr;
    addr.sin_port = htons(port->Tag);
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
    pthread_kill(evThread,SIGSELECT);
  }
  return _UNITTERM;

}

/*
Cannot have these as local variables in listen_fun under Mac OS X 10.5.2  !??
*/

struct sockaddr_in addr;
struct in_addr iaddr;

Closable_POSIX listen_fun (Sockets_POSIX this, Port_POSIX port, SOCKHANDLER handler, POLY self) {
  int sock = new_socket(handler);
  iaddr.s_addr = inet_addr("127.0.0.1");
  addr.sin_addr = iaddr;
  addr.sin_port = htons(port->Tag);
  addr.sin_family = AF_INET;
  if (bind(sock,(struct sockaddr *)&addr,sizeof(struct sockaddr)) < 0)
    perror("bind failed");
  listen(sock,5);
  FD_SET(sock,&readUsed);
  pthread_kill(evThread,SIGSELECT);
  return new_Closable(sock);
}

// ---------- Global env object ------------------------------------------------

struct DescState stdin_st       = { __GC__DescState, (Thread)0, (Thread)0, 0 };
struct DescState stdout_st      = { __GC__DescState, (Thread)0, (Thread)0, 1 };

struct DescClosable stdin_cl    = { __GC__Closable_POSIX, close_fun, &stdin_st };
struct DescClosable stdout_cl   = { __GC__Closable_POSIX, close_fun, &stdout_st };

struct File_POSIX stdin_file    = { __GC__File_POSIX, (Closable_POSIX)&stdin_cl, seek_fun };
struct File_POSIX stdout_file   = { __GC__File_POSIX, (Closable_POSIX)&stdout_cl, seek_fun };

struct RFile_POSIX stdin_rfile  = { __GC__RFile_POSIX, &stdin_file, read_fun, installR_fun };
struct WFile_POSIX stdout_wfile = { __GC__WFile_POSIX, &stdout_file, write_fun, installW_fun };

struct Sockets_POSIX tcp        = {__GC__Sockets_POSIX, connect_fun, listen_fun };

struct Internet_POSIX inet      = { __GC__Internet_POSIX, &tcp };

struct Env_POSIX env_struct     = { __GC__Env_POSIX, exit_fun,  NULL, &stdin_rfile, &stdout_wfile,
                                    openR_fun, openW_fun, &inet };

Env_POSIX env                   = &env_struct;

// ------- Copying for gc -----------------------------------------------

void copyEnvRoots (void) {
  int i;
  prog = (ACTION)copy((ADDR)prog);
  for(i = 0; i<maxDesc+1; i++) {
    if (rdTable[i]) rdTable[i] = (HANDLER)copy((ADDR)rdTable[i]);
    if (wrTable[i]) wrTable[i] = (ACTION)copy((ADDR)wrTable[i]);
    if (sockTable[i]) sockTable[i] = (SockData)copy((ADDR)sockTable[i]);
  }
	
}

// --------- Event loop ----------------------------------------------

void ev_handler () {
  return;
}

void *event_loop (void *arg) {
  fd_set readFds, writeFds;
  int i;
  while(1) {
    FD_COPY(&readUsed, &readFds);
    FD_COPY(&writeUsed, &writeFds);
    if (select(maxDesc+1, &readFds, &writeFds, NULL, NULL) >= 0) {
      for(i=0; i<maxDesc+1; i++) {
	if (FD_ISSET(i, &readFds)) {
	  if (rdTable[i]) {
	    LIST inp = read_descr(i);
	    if (inp) {
	      INTERRUPT_PROLOGUE();
	      rdTable[i]->Code(rdTable[i],inp,Inherit,Inherit);
	      INTERRUPT_EPILOGUE();
	    }
	    else if (sockTable[i]) { //we got a close message from peer on connected socket
	      SOCKHANDLER handler = sockTable[i]->handler;
	      Connection_POSIX conn = handler->Code(handler,new_Peer(i),NULL);
	      INTERRUPT_PROLOGUE();
	      conn->CONN2DEST->DEST2CLOSABLE->close_POSIX(conn->CONN2DEST->DEST2CLOSABLE,Inherit,Inherit);
	      INTERRUPT_EPILOGUE();
	      close(i);
	      CLR_RDTABLE(i);
	      sockTable[i] = NULL;
	    }
	  }
	  else if (sockTable[i]) { //listening socket received a connect request; we will accept
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
	    INTERRUPT_PROLOGUE();
	    wrTable[i]->Code(wrTable[i],Inherit,Inherit);
	    INTERRUPT_EPILOGUE();
	  }
	  else if (sockTable[i]) { //delayed connection has been accepted or has failed
	    int opt;
	    socklen_t len = sizeof(int);
	    FD_CLR(i,&writeUsed);
	    if (getsockopt(i,SOL_SOCKET,SO_ERROR, (void *)&opt, &len) < 0)
	      perror("getsockopt failed");
	    if (opt) {
	      netError(i,"Connection failed");
	    }
	    else {
	      setupConnection(i);
	    }
	  }
	}
      }
    }
  }
}

// --------- Initialization ----------------------------------------------------

void init_env (int argc, char **argv) {
  Int i;
  FD_ZERO(&readUsed);
  FD_ZERO(&writeUsed);
  pthread_create(&evThread, NULL, event_loop, NULL); 
  
  struct sigaction act;
  act.sa_flags = 0;
  sigemptyset( &act.sa_mask );
  act.sa_handler = ev_handler;
  sigaction( SIGSELECT, &act, NULL );
  Array arr; NEW(Array,arr,WORDS(sizeof(struct Array))+argc);
  SETGCINFO(arr,__GC__Array);
  arr->size = argc;
  for (i=0; i<argc; i++)
    arr->elems[i] = getStr(argv[i]);
  env->argv_POSIX = arr;
  fcntl(0, F_SETFL, O_NONBLOCK);
  fcntl(1, F_SETFL, O_NONBLOCK);
}
