#include <stdlib.h>
#include <mach-o/getsect.h>

#define allocwords(size)    (ADDR)malloc(size*sizeof(WORD))
#define HEAPSIZE            0x10000000                  // in words
#define STARTGC(hp,lim)     (hp >= 3 * (lim/8))
#define NEEDEXTRA(hp,lim)   (hp >= 7 * (lim/8))
#define ISWHITE(a)          (a >= heapMin && a < heapMax)
#define ISSTATIC(a)         (a <= edata)


WORD pagesize;                          // obtained from OS, measured in wors
ADDR base, lim, hp;                     // start, end, and current pos of latest "fromspace" (normal space)
ADDR base2, lim2, hp2;                  // start, end and current pos of "tospace" (only used during gc)
ADDR heapchain, heapMin, heapMax;       // chain of fromspaces formed by extension, with accumulated limits
WORD heapsize, thissize;                // accumulated size of all fromspaces, size of current fromspace
ADDR edata, rootp, scanp;               // end of static data, root pointer, scan pointer

WORD emergency = 0;                     // flag signalling heap overflow during gc

volatile ADDR scanning, copying;        // node currently being scanned (copied), normally null
                                        // (may be asynchronously reset by mutator)


void gcinit() {
        pagesize = sysconf(_SC_PAGESIZE) / sizeof(WORD);
        base = allocwords(HEAPSIZE);
        if (!base)
                panic("Cannot allocate initial heap");
        base[0] = 0;                    // first word in a heap is the "next" link
        hp = base + 1;
        lim = base + HEAPSIZE;
        heapchain = base;
        heapMin = base;
        heapMax = lim;
        heapsize = thissize = HEAPSIZE;
        base2 = lim2 = hp2 = (ADDR)0;   // no active tospace
        edata = (ADDR)get_end();
}

void extend(WORD size) {
        sigset_t previous_mask;
        DISABLE(&previous_mask);
        thissize = (size > HEAPSIZE-1 ? (((size-1)/pagesize)+1)*pagesize : HEAPSIZE);
        ADDR a = allocwords(thissize);
        if (!a)
                panic("Cannot allocate more memory");
        base[0] = (WORD)a;              // add link to new heap in first word of previous heap
        a[0] = 0;                       // null terminate chain of heaps
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

ADDR force2(WORD size) {                // Overflow in tospace
        DISABLE(NULL);
        emergency = 1;
        panic("Overlow in tospace");    // for now...
}


ADDR force(WORD size) {                 // Heap overflow in from-space...
        ADDR a;
        if (!hp2) {                     // GC not running, simply extend the heap
                extend(size);
                NEW(ADDR,a,size);
        } else {                        // GC is running, allocate in to-space and hope for the best
                NEW2(a,size);
        }
        return a;
}

ADDR copy(ADDR source) {
        if (!ISWHITE(source))                   // tospace address, or a low range constant
                return source;
        ADDR dest, info = GCINFO(source);
        if (!ISSTATIC(info))                    // gcinfo should point to static data;
                return copy(info);              // if not, we have a forwarding node
        WORD i, size = info[0];
        NEW2(dest,size);
        do {    copying = source;
                for (i=0; i<size; i++)
                        dest[i] = source[i];                
        } while (!CAS(source,0,&copying));
        source[0] = (WORD)dest;                 // make forwarded
        return dest;
}


ADDR scan(ADDR obj) {
        ADDR info = GCINFO(obj);
        if (!ISSTATIC(info)) {                  // gcinfo should point to static data;
                scan(info);                     // if not, we have a writ barrier (rescan request)
                return obj + 1;
        }
        do {    scanning = obj;
                WORD i = 1, offset = info[i];
                while (scanning && offset) {
                        obj[offset] = (WORD)copy((ADDR)obj[offset]);
                        offset = info[++i];
                }
        } while (!CAS(obj,0,&scanning));
        return obj + info[0];
}


void gc() {
        sigset_t previous_mask;
        DISABLE(&previous_mask);
        WORD level = hp - base + heapsize - thissize;
        if (!STARTGC(level,heapsize))
                return;
        if (NEEDEXTRA(level,heapsize))
                extend(HEAPSIZE);

        base2 = (ADDR)allocwords(heapsize);
        lim2 = base2 + heapsize;
        base2[0] = 0;
        hp2 = base2 + 1;
        ENABLE(&previous_mask);

        rootp = copy(rootp);
        scanp = base2 + 1;
        while (scanp != hp2)
                scanp = scan(scanp);
        
        do {
                base = heapchain;
                heapchain = (ADDR)base[0];
                free(base);
        } while (heapchain);
    
        DISABLE(&previous_mask);
        heapchain = base2;
        lim = lim2;
        hp = hp2;
        thissize = heapsize;
        base2 = lim2 = hp2 = (ADDR)0;
        ENABLE(&previous_mask);
}



void test(ADDR self) {
    PROLOGUE(self);
    LIST p;
    NEW(LIST,p,WORDS(3));
    EPILOGUE(self);
}

