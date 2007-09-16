TARGETS=tiny tiny.elf tiny.elf.stripped jonesforth

all: $(TARGETS)

jonesforth: jonesforth.S
	gcc -m32 -nostdlib -static -Wl,-Ttext,0 -o $@ $<

%: %.S elfheader.S
	gcc -Wl,--oformat -Wl,binary -nostdlib -o $@ $<

tiny.elf.stripped: tiny.elf
	cp tiny.elf tiny.elf.stripped
	strip tiny.elf.stripped

tiny.elf: tiny.S elfheader.S
	gcc -nostdlib -o tiny.elf tiny.S

clean:
	rm -f $(TARGETS)
