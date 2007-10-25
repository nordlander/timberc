// Marshalling and unmarshalling of strings

#include "timber.h"

void putStr(LIST xs) {
  Char x;
  while(1) {
    switch ((Int)xs) {
    case _NIL: 
      putchar('\n');
      return;
    default: 
      x = (Char)(Int)((CONS)xs)->a;
      xs = ((CONS)xs)->b;
      putchar(x);
      break;
    }
  }
}

LIST getStr(char *p) {
    if (!*p)
        return (LIST)_NIL;
    CONS n0; NEW(CONS, n0, sizeof(struct CONS));
    CONS n = n0;
    n->a = (POLY)(Int)*p++;
    while (*p) {
        NEW(LIST, n->b, sizeof(struct CONS));
        n = (CONS)n->b;
        n->a = (POLY)(Int)*p++;
    }
    n->b = (LIST)_NIL;
    return (LIST)n0;
}
