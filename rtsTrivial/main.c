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

LIST getStr(char *p) {
    if (!*p)
        return (LIST)_NIL;
    CONS n0 = NEW(sizeof(struct CONS));
    CONS n = n0;
    n->a = (POLY)*p++;
    while (*p) {
        n->b = NEW(sizeof(struct CONS));
        n = (CONS)n->b;
        n->a = (POLY)*p++;
    }
    n->b = (LIST)_NIL;
    return (LIST)n0;
}


int main(int argc, char **argv) {
    ROOTINIT();
    LIST w = (LIST)_NIL;
    for (; argc; argc--) {
            CONS n = NEW(sizeof(struct CONS));
            n->a = getStr(argv[argc-1]);
            n->b = w;
            w = (LIST)n;
    }
    putStr(ROOT(w));
}
