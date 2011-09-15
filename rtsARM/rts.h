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

#include "timber.h"

typedef struct {
	ADDR sp;
	ADDR cookie;
} arm7_context_t;

struct Thread {
    arm7_context_t context;     // machine state
	Thread next;                // for use in linked lists
    int thread_no;
    struct Msg *msg;            // message under execution
    Ref waitsFor;               // deadlock detection link
    int placeholders;           // for use during cyclic data construction
};

extern Thread current_thread;

#define CURRENT() current_thread

extern struct Thread threads[];


void IRQ_PROLOGUE(void);
void IRQ_EPILOGUE(void);

void GC_LOCK(OID obj);
void GC_UNLOCK(OID obj);


struct Scanner;
typedef struct Scanner *Scanner;

struct Scanner {
  void (*f) (); 
  Scanner next;
};

void addRootScanner(Scanner ls);
extern int rootsDirty;

void init_rts(int argc, char **argv);
void pruneStaticHeap();
void startup(Time_Time_to_Msg prog);



#endif
