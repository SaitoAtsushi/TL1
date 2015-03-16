
VPATH = runtime
CFLAGS = -std=c99 -O3 -flto
OBJECTS = arith.o compare.o function.o io.o logic.o system.o
PREFIX = /usr/local

%.o: %.c
	$(CC) -c $(CFLAGS) $< -o $@

all : libtl1rtl.a tl1c

libtl1rtl.a : $(OBJECTS)
	ar rv $@ $^

arith.o : arith.c runtime.h
compare.o : compare.c runtime.h
function.o : function.c runtime.h
io.o : io.c runtime.h
logic.o : logic.c runtime.h
system.o : system.c runtime.h
tl1c : tl1c.in
	sed -e "s#@sharedir@#$(PREFIX)/share#" -e "s#@bindir@#$(PREFIX)/bin#" <tl1c.in >tl1c

install : libtl1rtl.a tl1c
	install libtl1rtl.a $(PREFIX)/lib
	install -d $(PREFIX)/share/tl1
	install tl1c $(PREFIX)/bin
	install tl1toc.scm $(PREFIX)/bin
	install tl1/c-gen.sld tl1/exception.sld tl1/lexer.sld tl1/parser.sld tl1/pattern-match.sld tl1/peg.sld $(PREFIX)/share/tl1

clean :
	rm -f libtl1rtl.a tl1c $(OBJECTS) *~
