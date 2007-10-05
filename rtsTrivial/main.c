#include <stdio.h>
#include "../timber.h"

void ROOTINIT(void);
extern LIST ROOT;

void putStr(LIST xs) {
  Char x;
  while(1) {
    switch ((Int)xs) {
    case _NIL: 
      putchar('\n');
      return;
    default: 
      x = (Char)((CONS)xs)->hd;
      xs = ((CONS)xs)->tl;
      putchar(x);
      break;
    }
  }
}


int main(int argc, char **argv) {
    ROOTINIT();
    putStr(ROOT);
}
