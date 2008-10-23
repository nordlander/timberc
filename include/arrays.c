// Array primitives

#include "timber.h"

Array primListArray(BITS32 polytag, LIST xs) {
        Int len = 0;
        Int i = 0;
        LIST ys = xs;
        while ((Int)ys) {len++; ys = ((CONS)ys)->b;}; // not nice to compute the length here ...
        Array res; NEW(Array,res,WORDS(sizeof(struct Array))+len);
        res->GCINFO = (polytag ? __GC__Array1 : __GC__Array0);
        res->size = len;
        ys = xs;
        while((Int)ys) {res->elems[i] = ((CONS)ys)->a; ys = ((CONS)ys)->b; i++;}
        return res;
}

Array primUniArray(BITS32 polytag, Int len, POLY a) {
        Int i;
        Array res; NEW(Array,res,WORDS(sizeof(struct Array))+len);
        res->GCINFO = (polytag ? __GC__Array1 : __GC__Array0);
        res->size = len;
        for(i=0; i<len; i++) res->elems[i] = a;
        return res;
}

Array EmptyArray(BITS32 polytag, Int len) {
        Array res; NEW(Array,res,WORDS(sizeof(struct Array))+len);
        res->GCINFO = (polytag ? __GC__Array1 : __GC__Array0);
        res->size = len;
        return res;
}

Array CloneArray(BITS32 polytag, Array a, Int lev) {
        if (!lev)
                return a;
        Int i; 
        Array res; NEW(Array,res,WORDS(sizeof(struct Array))+a->size);
        res->size = a->size;
        lev--;
        if (lev) {
                res->GCINFO = __GC__Array0;
                for (i=0; i < a->size; i++) 
                        res->elems[i] = (ADDR)CloneArray(polytag, (Array)a->elems[i], lev);
        } else {
                res->GCINFO = (polytag ? __GC__Array1 : __GC__Array0);
                for (i=0; i < a->size; i++) 
                        res->elems[i] = a->elems[i];    
        }
        return res;
}

Array primUpdateArray(BITS32 polytag, Array a, Int i, POLY v) {
        a = CloneArray(polytag, a, 1);
        a->elems[i] = v;
        return a;
}
