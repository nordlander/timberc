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
