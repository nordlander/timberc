// The Timber compiler <timber-lang.org>
// 
// Copyright 2008-2012 Johan Nordlander <nordland@csee.ltu.se>
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

#include <avr/io.h>

#include "TinyTimber.h"


typedef struct {
    Object super;
    int state;
} C;

C obj = { initObject(), 0 };

void off( C *self ) {
    PORTC &= ~0x01;
}

void tick( C *self, int arg ) {
    PORTC |= 0x01;	   // On
    if (self->state)
        AFTER( MSEC(2), self, off, 0 );
    else
        AFTER( MSEC(1), self, off, 0 );
    AFTER( MSEC(20), self, tick, 0 );
    if (PINB & 0x10)
        PORTB = 0x01;
    else
        PORTB = 0x00;
}

void toggle( C *self, int arg ) {
    self->state = ~self->state;
    AFTER( SEC(1), self, toggle, 0 );
}

void startup(C *self) {
    DDRC = 0xFF; 
    DDRB = 0x01; 
    ASYNC(self, tick, 0); 
    ASYNC(self, toggle, 0); 
}

int main() {
    return TINYTIMBER(&obj, startup, 0);
}    
