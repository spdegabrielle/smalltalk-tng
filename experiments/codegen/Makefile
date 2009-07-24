shell: shell.c libdisasm.a
	gcc -DSTANDALONE=0 -I tinyscheme+cvs20090722 -o $@ $< \
		tinyscheme+cvs20090722/scheme.c \
		libdisasm.a

libdisasm.a:
	( cd libdisasm-0.23; ./configure --disable-shared --prefix=`pwd`/dist; make install )
	cp libdisasm-0.23/dist/lib/libdisasm.a .
	cp libdisasm-0.23/dist/include/libdis.h .

clean:
	rm -f t.o shell

dump: t.o
	otool -t t.o
	otool -tv t.o
