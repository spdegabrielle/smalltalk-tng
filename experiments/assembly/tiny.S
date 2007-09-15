#include <sys/syscall.h>
	.text
#include "elfheader.S"
	.globl _start
_start:
	movl	$1, %ebx
	movl	$hello, %ecx
	movl	$(helloend - hello), %edx
	movl	$__NR_write, %eax
	int	$0x80

	movl	$0, %ebx
	movl	$__NR_exit, %eax
	int	$0x80

hello:
	.ascii	"Hello, world!\n"
helloend:

fileend:
	.set	memsize, fileend - origin
