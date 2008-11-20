TIMBERTARGET = POSIX

TIMBERC      = ../timberc
TIMBERLIBS   = Prelude.t $(filter-out Prelude.t,$(notdir $(wildcard ../lib/*.t)))
OBJS         = $(TIMBERLIBS:.t=.o)

CFLAGS      += -D_GNU_SOURCE
CC           = gcc

DEST         = libTimber.a

all: $(DEST)

$(OBJS): %.o: ../lib/%.t ../include/timber.h
	$(TIMBERC) --api --target $(TIMBERTARGET) -c $<

$(DEST): $(OBJS) rts.o
	ar rc $(DEST) $(OBJS) rts.o

rts.o: rts.c cyclic.c gc.c env.c timer.c rts.h ../include/timber.h ../include/timber.c ../include/float.c
	$(CC) $(CFLAGS) -Wall -O2 -fno-strict-aliasing -g -I../include -I../lib -I. -c rts.c

install: $(DEST)
	$(INSTALL) -d $(DESTDIR)$(prefix)/rts$(TIMBERTARGET)
	$(INSTALL) $(DEST) $(DESTDIR)$(prefix)/rts$(TIMBERTARGET)
	$(INSTALL) timberc.cfg $(DESTDIR)$(prefix)/rts$(TIMBERTARGET)
	$(INSTALL) rts.h $(DESTDIR)$(prefix)/rts$(TIMBERTARGET)	
	$(INSTALL) main.c $(DESTDIR)$(prefix)/rts$(TIMBERTARGET)

clean:
	rm -f *.o $(DEST)

distclean:
	rm -f *.o timberc.cfg
