# Based on the teachings of http://www.muppetlabs.com/~breadbox/software/tiny/teensy.html

origin:
elfheader:			# Elf32_Ehdr
	.byte	0x7F
	.ascii	"ELF"
	.byte	1, 1, 1		# e_ident
	
	.byte	0,0,0,0
	.byte	0,0,0,0
	.byte	0
	.short	2		# e_type
	.short	3		# e_machine
	.long	1		# e_version
	.long	_start		# e_entry
	.long	programheader - elfheader # e_phoff
	.long	0		# e_shoff
	.long	0		# e_flags
	.short	elfheadersize	# e_ehsize
	.short	programheaderentrysize # e_phentsize
	.short	1		# e_phnum
	.short	40		# e_shentsize
	.short	0		# e_shnum
	.short	0		# e_shstrndx
elfheaderend:
	.set	elfheadersize, elfheaderend - elfheader

programheader:			# Elf32_Phdr
	.long	1  		# p_type
	.long	0		# p_offset
	.long	origin		# p_vaddr
	.long	origin		# p_paddr
	.long	fileend - origin# p_filesz
	.long	memsize		# p_memsz
	.long	5		# p_flags
	.long	0x1000		# p_align
programheaderentryend:
	.set	programheaderentrysize, programheaderentryend - programheader
