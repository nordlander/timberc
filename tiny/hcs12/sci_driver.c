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

#include <machine/hcs12.h>
#include "TinyTimber.h"
#include "sci_driver.h"


void sci_init(Serial *self, int unused) {
    self->port->scicr2 = 0;
    self->port->scibd = 156;                    // BAUD rate 9600 at 24 MHz
    self->port->scicr2 = RIE | RE | TE;         // Enable recieve, transmitt and recieve complete interrupt
  
    self->count = self->head = self->tail = 0;
}

static void outc(Serial *self, char c){
    if (self->count < SCI_BUFSIZE) {
        self->buf[self->head] = c;
        self->head = (self->head + 1) % SCI_BUFSIZE;
	    self->count++;
    }
}

void sci_write(Serial *self, char *p) {
    if (self->count == 0)
        self->port->scicr2 |= SCITIE;
    while (*p != '\0') {
        if (*p == '\n')
            outc(self, '\r');
        outc(self, *p++);
    }
}

void sci_writechar(Serial *self, int c) {
    if (self->count == 0)
        self->port->scicr2 |= SCITIE;
    outc(self, c);
}

void sci_interrupt(Serial *self, int unused) {
    if (self->port->scisr1 & RDRF) {            // Data recieved
        if (self->obj)
            ASYNC(self->obj, self->meth, self->port->scidrl);
    } else if (self->port->scisr1 & TDRE) {     // Transmitt buffer empty
        if (self->count > 0) {
            self->port->scidrl = self->buf[self->tail];
            self->tail = (self->tail + 1) % SCI_BUFSIZE;
            self->count--;
            if (self->count == 0)
	            self->port->scicr2 &= ~SCITIE;
        } else {
            self->port->scicr2 &= ~SCITIE;  
        }
    }
}

