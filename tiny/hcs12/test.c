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

#include <machine/hcs12.h>
#include <machine/hcs12/pim.h>
#include "sci_driver.h"
#include "can_driver.h"


#define PIM_PORT0   ((PPIM)(PTT))

typedef struct {
        Object super;
        int state;
        int count;
} Player;

Player player_obj = { initObject(), 0, 0 };

Object load_obj = initObject();

typedef struct {
    Object super;
    int count;
} App;

App app = { initObject(), 0 };

void reader(App*, int);
void receiver(App*, int);

Serial sci0 = initSerial(SCI_PORT0, &app, reader);

Can can0 = initCan(CAN0BASE, &app, receiver);

void dummy_workload(Object *self, int unused) {
    int i;
    for (i = 1; i <= 70; i++)
	    ;
    AFTER(USEC(1300), self, dummy_workload, 0);
}

void note_player(Player *self, int unused) {
    self->state = !self->state;
    PIM_PORT0->ptp = self->state ? 0x80 : 0x00;
    self->count++;
    if (self->count == 500) {
        self->count = 0;
        SCI_WRITE(&sci0, "*");
    }
    AFTER(USEC(500), self, note_player, 0);
//    SEND(USEC(500), USEC(50), self, note_player, 0);
}


void receiver(App *self, int unused) {
    CANMsg msg;
    CAN_RECEIVE(&can0, &msg);
    SCI_WRITE(&sci0, "Can msg received: ");
    SCI_WRITE(&sci0, msg.buff);
}

void ticker(App *self, int unused) {
    SCI_WRITE(&sci0, "!");
    AFTER(SEC(3), self, ticker, 0);
}

void reader(App *self, int c) {
    SCI_WRITE(&sci0, "Rcv: \'");
    SCI_WRITECHAR(&sci0, c);
    SCI_WRITE(&sci0, "\'\n");
    if (c == 't')
        AFTER(SEC(1), self, ticker, 0);
}

void startApp(App *self, int arg) {
    CANMsg msg;
    SCI_INIT(&sci0);
    CAN_INIT(&can0);
    SCI_WRITE(&sci0, "Hello, hello...\n");
    msg.msgId = 1;
    msg.nodeId = 1;
    msg.length = 6;
    msg.buff[0] = 'H';
    msg.buff[1] = 'e';
    msg.buff[2] = 'l';
    msg.buff[3] = 'l';
    msg.buff[4] = 'o';
    msg.buff[5] = 0;
    CAN_SEND(&can0, &msg);
    PIM_PORT0->ddrp = 0xFF;
    PIM_PORT0->ptp = 0x00;
//    ASYNC(&player_obj, note_player, 0);
//    ASYNC(&load_obj, dummy_workload, 0);
}

int main() {
    INSTALL(&sci0, sci_interrupt, SCI_IRQ0);
    INSTALL(&can0, can_interrupt, CAN_IRQ0);
    return TINYTIMBER(&app, startApp, 0);
}
