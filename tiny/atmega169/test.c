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
