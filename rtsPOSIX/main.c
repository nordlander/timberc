// The Timber compiler <timber-lang.org>
// 
// Copyright 2008 Johan Nordlander <nordland@csee.ltu.se>
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

#include "rts.h"
#include "timber.h"
#include "POSIX.h"
#include "env.h"

void ROOTINIT(void);
ACTION ROOT(Env_POSIX, POLY);

extern ACTION prog;
extern Env_POSIX env;

int main(int argc, char **argv) {
    init_rts(argc, argv);
    ROOTINIT();
    pruneStaticHeap();
    prog = ROOT(env, (POLY)0);
    prog->Code(prog,(POLY)Inherit,(POLY)Inherit);
    eventLoop();
}
