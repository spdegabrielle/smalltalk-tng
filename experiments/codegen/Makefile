CFLAGS+=-Wall -O2 -DSTANDALONE=0 -I tinyscheme+cvs20090722
LDFLAGS+=-O2 -DSTANDALONE=0 -I tinyscheme+cvs20090722

shell: shell.o libdisasm.a
	$(CC) $(LDFLAGS) -o $@ \
		$< \
		tinyscheme+cvs20090722/scheme.c \
		libdisasm.a

libdisasm.a:
	( cd libdisasm-0.23; ./configure --disable-shared --prefix=`pwd`/dist; make install )
	cp libdisasm-0.23/dist/lib/libdisasm.a .
	cp libdisasm-0.23/dist/include/libdis.h .

clean:
	rm -f t.o shell shell.o

dump: t.o
	otool -t t.o
	otool -tv t.o
