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
