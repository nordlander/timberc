#include <stdio.h>
#include "../timber.h"

void ROOTINIT(void);
extern LIST ROOT(LIST);

void putStr(LIST xs) {
  Char x;
  while(1) {
    switch ((Int)xs) {
    case _NIL: 
      putchar('\n');
      return;
    default: 
      x = (Char)((CONS)xs)->a;
      xs = ((CONS)xs)->b;
      putchar(x);
      break;
    }
  }
}


int main(int argc, char **argv) {
    ROOTINIT();
    putStr(ROOT((LIST)_NIL));
}
