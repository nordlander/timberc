#ifndef RTS_H_
#define RTS_H_



#define WORDS(bytes)      (((bytes)+sizeof(WORD)-1)/sizeof(WORD))
#define NEW(t,lhs,size)   lhs = (t)hp; hp += WORDS(size); if (hp >= lim) lhs = (t)force(WORDS(size));

extern ADDR hp, lim;
ADDR force(WORD);

UNITTYPE ASYNC(Msg, Time, Time);
UNITTYPE LOCK(PID);
UNITTYPE UNLOCK(PID);
POLY     RAISE(Int);

void putStr(LIST);
LIST getStr(char *);

void init_rts(void);

#endif
