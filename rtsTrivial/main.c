#include <stdio.h>
#include "../timber.h"

void ROOTINIT(void);
extern LIST ROOT;

void putStr(LIST xs) {
  POLY x;
  while(1) {
    switch (xs->tag) {
    case _CONS: 
      x = ((CONS)xs)->hd;
      xs = ((CONS)xs)->tl;
      putchar(((charbox)x)->Value);
      break;
    default: 
      putchar('\n');
      return;
    }
  }
}


int main(int argc, char **argv) {
    ROOTINIT();
    putStr(ROOT);
}
