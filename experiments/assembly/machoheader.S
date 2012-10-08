# Based on the teachings of http://www.muppetlabs.com/~breadbox/software/tiny/teensy.html
# and the content of https://developer.apple.com/library/mac/#documentation/developertools/conceptual/MachORuntime/Reference/reference.html

origin:
machoheader:
	.long	0xfeedfacf	# MH_MAGIC_64

	// Totally unfinished, so far. See tiny_osx.S.