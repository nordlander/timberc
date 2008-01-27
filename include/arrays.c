// Array primitives

#include "timber.h"

Array ListArray(LIST xs) {
        Int len = 0;
        Int i = 0;
        LIST ys = xs;
        while ((Int)ys) {len++; ys = ((CONS)ys)->b;}; // not nice to compute the length here ...
        Array res; NEW (Array,res,WORDS(sizeof(struct Array))+len);
        res->size = len;
        ys = xs;
        while((Int)ys) {res->elems[i] = ((CONS)ys)->a; ys = ((CONS)ys)->b; i++;}
        return res;
}

Array UniArray(Int len, POLY a) {
        Int i;
        Array res; NEW (Array,res,WORDS(sizeof(struct Array))+len);
        res->size = len;
        for(i=0; i<len; i++) res->elems[i] = a;
        return res;
}

Array EmptyArray(Int len) {
        Array res; NEW (Array,res,WORDS(sizeof(struct Array))+len); 
        res->size = len;
        return res;
}

Array CloneArray(Array a, Int lev) {
        if (!lev)
                return a;
        Int i; 
        Array res; NEW(Array,res,WORDS(sizeof(struct Array))+a->size);
        res->size = a->size;
        lev--;
        if (lev) for (i=0; i < a->size; i++) res->elems[i] = CloneArray(a->elems[i], lev);
        else     for (i=0; i < a->size; i++) res->elems[i] = a->elems[i];
        return res;
}

Array primUpdateArray(Array a, Int i, POLY v) {
        a = CloneArray(a, 1);
        a->elems[i] = v;
        return a;
}