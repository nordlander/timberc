#include <stdlib.h>
#include <mach-o/getsect.h>

#define allocwords(size)    malloc(size*sizeof(WORD))
#define HEAPSIZE            0x10000000
#define STARTGC(hp,lim)     (hp >= 3 * (lim/8))
#define NEEDEXTRA(hp,lim)   (hp >= 7 * (lim/8))
#define ISWHITE(a)          (a >= heapMin && a < heapMax)
#define ISSTATIC(a)         (a <= edata)


ADDR base, lim, hp;
ADDR base2, lim2, hp2;
ADDR heapchain, heapMin, heapMax;
WORD heapsize, thissize;
ADDR edata, rootp, scanp;

WORD emergency = 0;

volatile ADDR scanning, copying;

void gcinit() {
    base = (ADDR)allocwords(HEAPSIZE);
    base[0] = 0;
    hp = base + 1;
    lim = base + HEAPSIZE;
    heapchain = base;
    heapMin = base;
    heapMax = lim;
    heapsize = thissize = HEAPSIZE;
    base2 = lim2 = hp2 = (ADDR)0;
    edata = (ADDR)get_end();
}

void extend(WORD size) {
    thissize = (size > HEAPSIZE-1 ? size : HEAPSIZE);
    ADDR a = (ADDR)allocwords(thissize);
    if (!a)
        panic("Cannot allocate more memory");
    base[0] = (WORD)a;
    a[0] = 0;
    base = a;
    lim = a + thissize;
    hp = a + 1;
    heapsize += thissize;
    if (base < heapMin)
        heapMin = base;
    if (lim > heapMax)
        heapMax = lim;
}

ADDR force2(WORD size) {                // Overflow in to-space
    emergency = 1;
    panic("Overlow in to-space");       // for now...
}


ADDR force(WORD size) {                 // Heap overflow in from-space...
    ADDR a;
    if (!hp2) {                         // GC not running, simply extend the heap
        extend(size);
        a = hp;
        hp += size;
    } else {                            // GC is running, allocate in to-space and hope for the best
        NEW2(a,size);
    }
    return a;
}

ADDR copy(ADDR source) {
    if (!ISWHITE(source))
        return source;
    ADDR info = GCINFO(source);
    if (!ISSTATIC(info))                        // is forwarded
        return copy(info);
    WORD size = info[0];
    DISABLE();
    ADDR dest = hp2;
    hp2 += size;
    if (hp2 >= lim2)
        ;
    while (!copying) {
        copying = source;
        ENABLE();
        WORD i;
        for (i=0; i<size; i++)
            dest[i] = source[i];
        DISABLE();
    }
    copying = 0;
    ENABLE();
    source[0] = (WORD)dest;                     // make forwarded
    return dest;
}


ADDR scan(ADDR obj) {
    ADDR info = GCINFO(obj);
    if (!ISSTATIC(info)) {                      // is write barrier
        scan(info);
        return obj + 1;
    }
    DISABLE();
    while (!scanning) {
        scanning = obj;
        ENABLE();
        WORD i = 1, offset = info[i];
        while (scanning && offset) {
            obj[offset] = (WORD)copy((ADDR)obj[offset]);
            offset = info[++i];
        }
        DISABLE();
    }
    scanning = 0;
    ENABLE();
    return obj + info[0];
}


void gc() {
    DISABLE();
    WORD level = hp - base + heapsize - thissize;
    if (!STARTGC(level,heapsize))
        return;
    if (NEEDEXTRA(level,heapsize))
        extend(HEAPSIZE);

    base2 = (ADDR)malloc(heapsize);
    lim2 = base2 + heapsize;
    base2[0] = 0;
    hp2 = base2 + 1;

    ENABLE();
    rootp = copy(rootp);
    scanp = base2 + 1;
    while (scanp != hp2)
        scanp = scan(scanp);
        
    do {
        base = heapchain;
        heapchain = (ADDR)base[0];
        free(base);
    } while (heapchain);
    
    DISABLE();
    heapchain = base2;
    lim = lim2;
    hp = hp2;
    thissize = heapsize;
    base2 = lim2 = hp2 = (ADDR)0;
    ENABLE();
}



void test(ADDR self) {
    PROLOGUE(self);
    LIST p;
    NEW(LIST,p,WORDS(3));
    EPILOGUE(self);
}

