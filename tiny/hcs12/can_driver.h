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

#ifndef CAN_DRIVER_H
#define CAN_DRIVER_H

#include "TinyTimber.h"

typedef unsigned char uchar;

typedef struct {
  uchar msgId;  //Valid values: 0-127
  uchar nodeId; //Valid values: 0-15
  uchar length;
  uchar buff[8];
} CANMsg;

extern struct tagCAN;

#define CAN_BUFSIZE 8

typedef struct {
  Object  super; 
  struct tagCAN *port;
  Object  *obj;
  Method  *meth;
  char    tail;
  char    head;
  char    count;
  CANMsg  iBuff[CAN_BUFSIZE];
} Can;

#define initCan(port, obj, meth)  { initObject(), port, (Object*)obj, (Method)meth, 0, 0, 0}

#define CAN_PORT0   (struct tagCAN *)CAN0BASE
#define CAN_PORT1   (struct tagCAN *)CAN1BASE

#define CAN_IRQ0    IRQ_MSCAN0Rx
#define CAN_IRQ1    IRQ_MSCAN1Rx

#define CAN_INIT(can)               SYNC(can, can_init, 0)
#define CAN_RECEIVE(can, msgptr)    SYNC(can, can_receive, msgptr)
#define CAN_SEND(can, msgptr)       SYNC(can, can_send, msgptr)

void can_init(Can *obj, int unused);
int can_receive(Can *self, CANMsg *msg);
int can_send(Can *self, CANMsg *msg);

void can_interrupt(Can *self, int unused);

#endif
