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

#include "config.h"
#include "timber.h"

static inline void PROTECT(int state) {
	int tmp;
	asm volatile(
		"cmp	%1, #0\n"
		"mrs	%0, CPSR\n"
		"orrne	%0, %0, #0x80|0x40\n"
		"biceq	%0, %0, #0x80|0x40\n"
		"msr	CPSR_c, %0\n"
		: "=r" (tmp)
		: "r" (state)
		);
}

static inline int ISPROTECTED(void) {
	int tmp;
	asm volatile(
			"mrs	%0, CPSR\n"
			"and	%0, %0, #0x80|0x40\n"
			: "=r" (tmp)
			);
	return tmp;
}


typedef struct {
	unsigned int *sp;
	unsigned int *cookie;
} arm7_context_t;

struct Thread {
    arm7_context_t context;     // machine state
	Thread next;                // for use in linked lists
    struct Msg *msg;            // message under execution
    Ref waitsFor;               // deadlock detection link
    int placeholders;           // for use during cyclic data construction
};

extern Thread current_thread;

#define CURRENT() current_thread


/*
#define NEW(t,addr,words)       { int status = ISPROTECTED(); \
                                  PROTECT(1); \
	                              addr = (t)hp; \
	                              hp = (ADDR)addr+(words); \
                                  if (hp >= lim) force(words,(ADDR)addr); \
		                          PROTECT(status); \
                                }
*/


struct Scanner;
typedef struct Scanner *Scanner;

struct Scanner {
  void (*f) (); 
  Scanner next;
};

void addRootScanner(Scanner ls);
extern int rootsDirty;

void init_rts(void);
void mainCont();
void pruneStaticHeap();



#endif
