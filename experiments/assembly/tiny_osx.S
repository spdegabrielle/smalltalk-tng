#include <sys/syscall.h>
	.text
	.code64

//#include "machoheader.S"
origin:

	.globl start
start:
	// 16-byte stack alignment for system calls??
	andq	$0xfffffffffffffff0, %rsp

	movq	$1, %rdi
	leaq	hello(%rip), %rsi
	movl	hellolen(%rip), %edx
	movq	$(0x2000000 | SYS_write), %rax
	syscall

	movq	$0, %rdi
	movq	$(0x2000000 | SYS_exit), %rax
	syscall

hello:
	.ascii	"Hello, world!\n"
hellolen:
	.long	. - hello

fileend:
	.set	memsize, fileend - origin
