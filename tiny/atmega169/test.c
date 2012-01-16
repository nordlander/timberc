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

#include "TinyTimber.h"
#include "LCD.h"
#include <avr/io.h>


//------------------------------------------------

typedef struct {
    Object super;
    long val;
    Msg pending;
} Counter;

#define initCounter(v)  { initObject(), v, NULL }

void inc( Counter *self, int arg ) {
    if (!self->pending) {
        writeLong( self->val );
    }
    self->val++;
    AFTER( MSEC(100), self, inc, 0 );
}

void enddisable( Counter *self, int arg ) {
    self->pending = NULL;
}

void tempdisable( Counter *self, int arg ) {
    if (self->pending)
        ABORT(self->pending);
    self->pending = AFTER( SEC(1), self, enddisable, 0 );
}

void reset( Counter *self, int arg ) {
    self->val = 0;
    clearLCD();
    writeLong(0);
}

Counter c = initCounter(0);

//------------------------------------------------

Object app = initObject();

void startup(Object *self, int arg) {
    EIMSK = 0xC0;
    PCMSK1 = 0xD0;
    PCMSK0 = 0x0C;
    DDRB = 0x2F;
    DDRE = 0xF3;
    PORTB = 0xD0;
    PORTE = 0x0C;
    initLCD();
    ASYNC(&c, inc, 0);
}

void change0(Object *self, int arg) {
    
}

void change1(Object *self, int arg) {
    if (!(PINB & 0x80)) 
        ASYNC(&c, tempdisable, 0); 
    else if (!(PINB & 0x10)) 
        ASYNC(&c, reset, 0);
}

int main() {
    INSTALL(&app, change0, IRQ_PCINT0);
    INSTALL(&app, change1, IRQ_PCINT1);
    return TINYTIMBER(&app, startup, 0);
}
