#include <stdlib.h>
#include <stdio.h>
#include "rts.h"


#define allocwords(size)   malloc(size*sizeof(WORD))
#define HEAPSIZE           0x100000


ADDR base, lim, hp;
WORD heapsize, thissize;


void panic(void) {
        fprintf(stderr, "Timber RTS cannot allocate more memory, quitting...\n");
}

void init_rts(void) {
    base = (ADDR)allocwords(HEAPSIZE);
    base[0] = 0;
    hp = base + 1;
    lim = base + HEAPSIZE;
    heapsize = thissize = HEAPSIZE;
}

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

void extend(WORD size) {
    thissize = (size > HEAPSIZE-1 ? size : HEAPSIZE);
    ADDR a = (ADDR)allocwords(thissize);
    if (!a)
        panic();
    base[0] = (WORD)a;
    a[0] = 0;
    base = a;
    lim = a + thissize;
    hp = a + 1;
    heapsize += thissize;
}

ADDR force(WORD size) {                 // Heap overflow in from-space...
    ADDR a;
    extend(size);
    a = hp;
    hp += size;
    return a;
}

POLY RAISE(Int err) {
    fprintf(stderr, "Unhandled exception no. %d\n", err);
    exit(1);
}

Array primListArray(LIST xs) {
  Int len = 0;
  Int i = 0;
  LIST ys = xs;
  while ((Int)ys) {len++; ys = ((CONS)ys)->b;}; // not nice to compute the length here ...
  Array res; NEW (Array,res,sizeof(Int)+ len*sizeof(POLY)); 
  res->size = len;
  ys = xs;
  while((Int)ys) {res->elems[i] = ((CONS)ys)->a; ys = ((CONS)ys)->b; i++;}
  return res;
}

Array primConstArray(Int len, POLY a) {
  Int i;
  Array res; NEW (Array,res,sizeof(Int)+ len*sizeof(POLY)); 
  res->size = len;
  for(i=0; i<len; i++) res->elems[i] = a;
  return res;
}

Array primCloneArray(Array a, Int lev) {
  switch (lev) {
     case 0: return a;
     default: {
       Array res; NEW(Array,res,sizeof(Int)+ a->size * sizeof(POLY));
       res->size = a->size;
       Int i; for(i=0; i < a->size; i++) res->elems[i] = primCloneArray(a->elems[i],lev-1);
       return res;
     }
  }
}
