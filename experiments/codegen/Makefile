shell: shell.c
	gcc -DSTANDALONE=0 -I tinyscheme+cvs20090722 -o $@ $< tinyscheme+cvs20090722/scheme.c

clean:
	rm -f t.o shell

dump: t.o
	otool -t t.o
	otool -tv t.o
