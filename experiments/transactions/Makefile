all: splay-tree.so

clean:
	rm -f splay-tree.so

%.so: %.scm
	csc -O3 -s $<
