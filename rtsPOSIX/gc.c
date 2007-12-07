#include <stdlib.h>
#include <mach-o/getsect.h>

#define NEW2(addr,size)         { ADDR top; do { addr = hp2; top = (ADDR)addr + WORDS(size); } while (!CAS(addr,top,&hp2)); \
                                  if (top >= lim2) addr = force2(WORDS(size)); }

#define GCINFO(obj)             ((ADDR)obj)[0]
#define GC_PROLOGUE(obj)        { if (ISFORWARD(GCINFO(obj))) obj = (PID)GCINFO(obj); }                         // read barrier
#define GC_EPILOGUE(obj)        { if (hp2) { \
                                      if (!GCINFO(obj)) GCINFO(obj) = 1; \
                                      if (ISBLACK(obj)) { ADDR a; NEW2(a,1); a[0] = (WORD)obj; } } }    // write barrier
#define TIMERQ_PROLOGUE()       { if (timerQchanged) timerQ = timerQchanged; }                          // read barrier
#define TIMERQ_EPILOGUE()       { timerQchanged = timerQ; }                                             // write barrier

#define allocwords(size)        (ADDR)malloc(size*sizeof(WORD))
#define HEAPSIZE                0x10000000                  // in words
#define STARTGC(hp,lim)         ((hp) >= 3 * ((lim)/8))
#define NEEDEXTRA(hp,lim)       ((hp) >= 7 * ((lim)/8))
#define ISWHITE(a)              ((ADDR)(a) >= heapMin && (ADDR)(a) < heapMax)
#define ISBLACK(a)              ((ADDR)(a) >= base2 && (ADDR)(a) < scanp)
#define ISFORWARD(a)            ((ADDR)(a) > edata)


WORD pagesize;                          // obtained from OS, measured in wors
ADDR base, lim, hp;                     // start, end, and current pos of latest "fromspace" (normal space)
ADDR base2, lim2, hp2;                  // start, end and current pos of "tospace" (only used during gc)
ADDR heapchain, heapMin, heapMax;       // chain of fromspaces formed by extension, with accumulated limits
WORD heapsize, thissize;                // accumulated size of all fromspaces, size of current fromspace
ADDR edata, scanp;                      // end of static data, scan pointer (only used during gc)

Msg timerQchanged = 0;                  // ptr signalling the need to rescan the timerQ 

char emergency = 0;                     // flag signalling heap overflow during gc


void gcinit() {
        pagesize = sysconf(_SC_PAGESIZE) / sizeof(WORD);
        base = allocwords(HEAPSIZE);
        if (!base)
                panic("Cannot allocate initial heap");
        base[0] = 0;                                    // first word in a heap is the "next" link
        hp = base + 1;
        lim = base + HEAPSIZE;
        heapchain = base;
        heapMin = base;
        heapMax = lim;
        heapsize = thissize = HEAPSIZE;
        base2 = lim2 = hp2 = (ADDR)0;                   // no active tospace
        edata = (ADDR)get_end();
}

void extend(WORD size) {
        sigset_t previous_mask;
        DISABLE(&previous_mask);
        thissize = (size > HEAPSIZE-1 ? (((size-1)/pagesize)+1)*pagesize : HEAPSIZE);
        ADDR a = allocwords(thissize);
        if (!a)
                panic("Cannot allocate more memory");
        base[0] = (WORD)a;                              // add link to new heap in first word of previous heap
        a[0] = 0;                                       // null terminate chain of heaps
        base = a;
        lim = a + thissize;
        hp = a + 1;
        heapsize += thissize;
        if (base < heapMin)
                heapMin = base;
        if (lim > heapMax)
                heapMax = lim;
        ENABLE(&previous_mask);
}

ADDR force2(WORD size) {                                // Overflow in tospace
        DISABLE(NULL);
        emergency = 1;
        panic("Overlow in tospace");                    // for now...
}


ADDR force(WORD size) {                                 // Heap overflow in from-space...
        ADDR a;
        if (!hp2) {                                     // GC not running, simply extend the heap
                extend(size);
                NEW(ADDR,a,size);
        } else {                                        // GC is running, allocate in to-space and hope for the best
                NEW2(a,size);
        }
        return a;
}

ADDR copy(ADDR obj) {
        if (!ISWHITE(obj))                              // don't copy if obj is a tospace address or a low range constant
                return obj;
        ADDR dest, info = (ADDR)GCINFO(obj);
        if (ISFORWARD(info))                            // gcinfo should point to static data;
                return info;                            // if not, we have a forward ptr
        WORD i, size = info[0];
        NEW2(dest,size);
        GCINFO(dest) = (WORD)info;                      // gcinfo ptr is immutable
        do {    GCINFO(obj) = 0;                        // flag copying in progress by nulling out gcinfo ptr
                for (i=1; i<size; i++)
                        dest[i] = obj[i];                
        } while (!CAS(0,(WORD)dest,&GCINFO(obj)));      // repeat copying if dirty, else set forward ptr
        return dest;
}


ADDR scan(ADDR obj) {
        ADDR info = (ADDR)GCINFO(obj);
        if (ISFORWARD(info)) {                          // gcinfo should point to static data;
                scan(info);                             // if not, we have a write barrier (rescan request)
                return obj + 1;
        }
        do {    GCINFO(obj) = 0;                        // flag scanning in progress by nulling out gcinfo ptr
                WORD i = 1, offset = info[i];
                while (offset && !GCINFO(obj)) {
                        obj[offset] = (WORD)copy((ADDR)obj[offset]);
                        offset = info[++i];
                }
        } while (!CAS(0,(WORD)info,&GCINFO(obj)));      // repeat scanning if dirty, else reinstall gcinfo ptr
        return obj + info[0];
}

void copyTimerQ() {
        Msg old, new, new0;
        do {    timerQchanged = 0;
                old = timerQ;
                new0 = new = (Msg)copy((ADDR)old);
                while (old) {
                        new->next = (Msg)copy((ADDR)old->next);
                        new = new->next;
                        old = old->next;
                }
        } while (!CAS(0,new0,&timerQchanged));          // an interrupt immediately after this test could be dangerous
        timerQ = new0;                                  // luckily the mutator uses timerQchanged instead of timerQ if it is set
}

void gc() {
        sigset_t previous_mask;
        DISABLE(&previous_mask);

        WORD level = hp - base + heapsize - thissize;
        if (!STARTGC(level,heapsize)) {
                ENABLE(&previous_mask);
                return;
        }

        if (NEEDEXTRA(level,heapsize))
                extend(HEAPSIZE);
        base2 = (ADDR)allocwords(heapsize);             // allocate tospace, with size equal to sum of all fromspace heaps
        lim2 = base2 + heapsize;
        base2[0] = 0;
        hp2 = base2 + 1;
        scanp = hp2;
        ENABLE(&previous_mask);

        copyEnvRoots();
        copyTimerQ();
        while (scanp != hp2)
                scanp = scan(scanp);

        DISABLE(&previous_mask);
        do {    base = heapchain;
                heapchain = (ADDR)base[0];
                free(base);
        } while (heapchain);
        heapchain = base2;
        lim = lim2;
        hp = hp2;
        thissize = heapsize;
        base2 = lim2 = hp2 = (ADDR)0;
        ENABLE(&previous_mask);
}

