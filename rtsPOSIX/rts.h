#ifndef RTS_H_
#define RTS_H_

#define WORDS(bytes)        (((bytes)+sizeof(WORD)-1)/sizeof(WORD))
#define ISBLACK(a)          (a >= base2 && a < scanp)

#define GCINFO(addr)        ((ADDR)addr[0])
#define NEW(t,addr,size)    { DISABLE(); addr = (t)hp; hp += WORDS(size); if (hp >= lim) addr = (t)force(WORDS(size)); ENABLE(); }
#define NEW2(addr,size)     { DISABLE(); addr = hp2; hp2 += WORDS(size); if (hp2 >= lim2) addr = force2(WORDS(size)); ENABLE(); }
#define PROLOGUE(obj)       { if (GCINFO(obj)[0] < 0) obj = (ADDR)obj[1]; }
#define EPILOGUE(obj)       { if (hp2) { \
                                 if (obj==scanning) scanning = 0; \
                                 if (obj==copying) copying = 0; \
                                 if (ISBLACK(obj)) { ADDR a; NEW2(a,1); a[0] = (WORD)obj; } } }     // write barrier


extern ADDR hp, lim, hp2, lim2, base2, scanp;
extern volatile ADDR scanning, copying;

ADDR force(WORD);

UNITTYPE ASYNC(Msg, Time, Time);
UNITTYPE LOCK(PID);
UNITTYPE UNLOCK(PID);
POLY     RAISE(Int);

void putStr(LIST);
LIST getStr(char *);

void init_rts(void);

#endif
