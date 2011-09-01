// The Timber compiler <timber-lang.org>
// 
// Copyright 2008-2009 Johan Nordlander <nordland@csee.ltu.se>
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
// 
// 3. Neither the names of the copyright holder and any identified
//    contributors, nor the names of their affiliations, may be used to 
//    endorse or promote products derived from this software without 
//    specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
// OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
// OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
// STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
// ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

#ifndef RTS_H_
#define RTS_H_

#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <sys/time.h>
#include "config.h"
#include "timber.h"

#define DISABLE(mutex)  pthread_mutex_lock(&mutex)
#define ENABLE(mutex)   pthread_mutex_unlock(&mutex)
#define TIMERGET(x)     gettimeofday(&x, NULL)


typedef struct Thread *Thread;

struct Thread {
   Thread next;            // for use in linked lists
   Msg msg;                // message under execution
   int prio;
   pthread_t id;
   int index;
   pthread_cond_t trigger;
   int placeholders;       // for use during cyclic data construction
};

extern struct Thread threads[];
extern pthread_mutex_t rts;
extern pthread_mutexattr_t glob_mutexattr;
extern pthread_key_t current_key;
extern AbsTime absInfinity;
extern Time timeZero;


#define SIGSELECT SIGUSR1

Thread runAsSeparateThread(void(*fun)(Thread), Msg m);
void runAsMainContinuation(void(*fun)(void));



struct Scanner;
typedef struct Scanner *Scanner;

struct Scanner {
  void (*f) (); 
  Scanner next;
};

void addRootScanner(Scanner ls);
extern int rootsDirty;

int getArgc();
char **getArgv();
void init_rts(int argc, char **argv);
void mainCont();
void pruneStaticHeap();

#endif
