all: tiny tiny.elf tiny.elf.stripped

tiny: tiny.S elfheader.S
	gcc -Wl,--oformat -Wl,binary -nostdlib -o tiny tiny.S

tiny.elf.stripped: tiny.elf
	cp tiny.elf tiny.elf.stripped
	strip tiny.elf.stripped

tiny.elf: tiny.S elfheader.S
	gcc -nostdlib -o tiny.elf tiny.S

clean:
	rm -f tiny tiny.elf tiny.elf.stripped
