#include "timber.h"

struct DummyStruct {
  WORD *gcinfo;
  Int n;
};

typedef struct DummyStruct *DummyStruct;

static WORD __GC__DummyStruct[] = {WORDS(sizeof(struct DummyStruct)+20), 0};

LIST primShowFloat(Float x) {
  DummyStruct p;
  NEW(DummyStruct,p,sizeof(struct DummyStruct)+20);
  SETGCINFO(p,__GC__DummyStruct);
  snprintf((char*)&p->n,20,"%e",x);
  LIST res = getStr((char*)&p->n);
  return res;
}
 
