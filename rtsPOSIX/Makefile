TIMBERTARGET = POSIX

TIMBERC      = ../timberc
TIMBERLIBS   = Prelude.t $(filter-out Prelude.t,$(notdir $(wildcard ../lib/*.t)))
OBJS         = $(TIMBERLIBS:.t=.o)


DEST         = libTimber.a

all: $(DEST)

$(OBJS): %.o: ../lib/%.t ../include/timber.h
	$(TIMBERC) --target $(TIMBERTARGET) -c $<

$(DEST): $(OBJS) rts.o
	libtool -static -o $(DEST) $(OBJS) rts.o

rts.o: rts.c gc.c env.c rts.h ../include/timber.h
	$(CC) $(CFLAGS) -I../include -I../lib -I. -c rts.c

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
