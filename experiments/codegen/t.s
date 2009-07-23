	.text
_foo:
/*	movl	4(%eax), %eax */

	// 8B ========

	// done
	movl	131071(%eax), %eax // 80		10000000
	movl	131071(%eax), %ecx // 88		10001000
	movl	131071(%eax), %edx // 90		10010000
	movl	131071(%eax), %ebx // 98		10011000
	movl	131071(%eax), %esp // a0		10100000
	movl	131071(%eax), %ebp // a8		10101000
	movl	131071(%eax), %esi // b0		10110000
	movl	131071(%eax), %edi // b8		10111000

	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	
	// done
	movl	131071(%eax), %eax // 80		10000000
	movl	131071(%ecx), %eax // 81		10000001
	movl	131071(%edx), %eax // 82		10000010
	movl	131071(%ebx), %eax // 83		10000011
	movl	131071(%esp), %eax // 84 24		10000100 00100100
	movl	131071(%ebp), %eax // 85		10000101
	movl	131071(%esi), %eax // 86		10000110
	movl	131071(%edi), %eax // 87		10000111

	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	
	// done
	movl	127(%eax), %eax // 40			01000000
	movl	127(%eax), %ecx // 48			01001000
	movl	127(%eax), %edx // 50			01010000
	movl	127(%eax), %ebx // 58			01011000
	movl	127(%eax), %esp // 60			01100000
	movl	127(%eax), %ebp // 68			01101000
	movl	127(%eax), %esi // 70			01110000
	movl	127(%eax), %edi // 78			01111000

	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	
	// done
	movl	127(%eax), %eax // 40			01000000
	movl	127(%ecx), %eax // 41			01000001
	movl	127(%edx), %eax // 42			01000010
	movl	127(%ebx), %eax // 43			01000011
	movl	127(%esp), %eax // 44			01000100
	movl	127(%ebp), %eax // 45			01000101
	movl	127(%esi), %eax // 46			01000110
	movl	127(%edi), %eax // 47			01000111

	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	
	// done
	movl	(%eax), %eax // 00			00000000
	movl	(%ecx), %eax // 01			00000001
	movl	(%edx), %eax // 02			00000010
	movl	(%ebx), %eax // 03			00000011
	movl	(%esp), %eax // 04 24			00000100 00100100
	movl	(%ebp), %eax // 45 00 (!)		01000101 00000000
	movl	(%esi), %eax // 06			00000110
	movl	(%edi), %eax // 07			00000111

	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop

	// done
	movl	(%eax), %eax // 00			00000000
	movl	(%eax), %ecx // 08			00001000
	movl	(%eax), %edx // 10			00010000
	movl	(%eax), %ebx // 18			00011000
	movl	(%eax), %esp // 20			00100000
	movl	(%eax), %ebp // 28			00101000
	movl	(%eax), %esi // 30			00110000
	movl	(%eax), %edi // 38			00111000

	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop

	// done
	movl	0xabcdabcd, %eax // A1 m
	movl	0xabcdabcd, %ecx // 8B 0D m		00001101
	movl	0xabcdabcd, %edx // 8B 15 m		00010101
	movl	0xabcdabcd, %ebx // 8B 1D m		00011101
	movl	0xabcdabcd, %esp // 8B 25 m		00100101
	movl	0xabcdabcd, %ebp // 8B 2D m		00101101
	movl	0xabcdabcd, %esi // 8B 35 m		00110101
	movl	0xabcdabcd, %edi // 8B 3D m		00111101

	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop

	// done
	movl	%eax, 0x00efefef // A3 m
	movl	%ecx, 0x00efefef // 89 0D m		00001101
	movl	%edx, 0x00efefef // 89 15 m		00010101
	movl	%ebx, 0x00efefef // 89 1D m		00011101
	movl	%esp, 0x00efefef // 89 25 m		00100101
	movl	%ebp, 0x00efefef // 89 2D m		00101101
	movl	%esi, 0x00efefef // 89 35 m		00110101
	movl	%edi, 0x00efefef // 89 3D m		00111101

	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop

	// done
	movl	$0x3e3e3e3e, %eax // B8 i		10111000
	movl	$0x3e3e3e3e, %ecx // B9 i
	movl	$0x3e3e3e3e, %edx // BA i
	movl	$0x3e3e3e3e, %ebx // BB i
	movl	$0x3e3e3e3e, %esp // BC i
	movl	$0x3e3e3e3e, %ebp // BD i
	movl	$0x3e3e3e3e, %esi // BE i
	movl	$0x3e3e3e3e, %edi // BF i

	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop

	// done
	movl	127(%eax), %eax // 40			01000000
	movl	127(%ecx), %ecx // 49			01001001
	movl	127(%edx), %edx // 52			01010010
	movl	127(%ebx), %ebx // 5B			01011011
	movl	127(%esp), %esp // 64 24		01100100
	movl	127(%ebp), %ebp // 6D			01101101
	movl	127(%esi), %esi // 76			01110110
	movl	127(%edi), %edi // 7F			01111111

	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop

	// 89 ========

	// done
	movl	%eax, %eax // C0
	movl	%eax, %ecx // C1
	movl	%eax, %edx // C2
	movl	%eax, %ebx // C3
	movl	%eax, %esp // C4
	movl	%eax, %ebp // C5
	movl	%eax, %esi // C6			11000110
	movl	%eax, %edi // C7			11000111

	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop

	// done
	movl	%eax, %eax // C0
	movl	%ecx, %eax // C8
	movl	%edx, %eax // D0
	movl	%ebx, %eax // D8
	movl	%esp, %eax // E0
	movl	%ebp, %eax // E8
	movl	%esi, %eax // F0			11110000
	movl	%edi, %eax // F8			11111000

	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop

	movb	(%eax), %eax // 8A 00
	movb	0x66(%eax), %eax // 8A 40 66
	movb	0x666666(%eax), %eax // 8A 80 66 66 66 00
	movb	(%esp), %edx // 8A 14 24
	movb	0x66(%esp), %edx // 8A 54 24 66
	movb	0x666666(%esp), %edx // 8A 94 24 66 66 66 00
	movb	(%esi), %ebx // 8A 1E
	movb	0x66(%esi), %ebx // 8A 5E 66
	movb	0x666666(%esi), %ebx // 8A 9E 66 66 66 00

	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop

	pushl	%eax // 50		01010000
	pushl	%ecx
	pushl	%edx
	pushl	%ebx
	pushl	%esp
	pushl	%ebp
	pushl	%esi
	pushl	%edi // 57		01010111
	popl	%eax // 58		01011000
	popl	%ecx
	popl	%edx
	popl	%ebx
	popl	%esp
	popl	%ebp
	popl	%esi
	popl	%edi // 5F		01011111

	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop

	// 83 ec 0c			11101100
	// 83 e8 0c			11101000
	// 83 eb 0c			11101011
	subl	$12, %esp
	subl	$12, %eax
	subl	$12, %ebx

	// 83 c4 0c			11000100
	// 83 c0 0c			11000000
	// 83 c3 0c			11000011
	addl	$12, %esp
	addl	$12, %eax
	addl	$12, %ebx

	// 81 ec ff ff 01 00
	// 2d ff ff 01 00
	// 81 eb ff ff 01 00
	subl	$131071, %esp
	subl	$131071, %eax
	subl	$131071, %ebx

	// 81 c4 ff ff 01 00
	// 05 ff ff 01 00
	// 81 c3 ff ff 01 00 
	addl	$131071, %esp
	addl	$131071, %eax
	addl	$131071, %ebx

	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop

//	movb	%bl, %al
//	movw	%bx, %ax
//	movl	%ebx, %eax

/*
	movl	0x34343434, %eax
	movb	0x34343434, %eax
	movl	x, %eax
	movb	x, %eax
x:
	*/

	subl	$12, %edx	// 83 ea 0c
	subb	$12, %edx	// 80 ea 0c
	subl	$131071, %edx	// 81 ea ff ff 01 00

	addl	$25, %edx
	addb	$25, %edx
	addl	$131071, %edx

	addl	$250, %edx
	addb	$250, %edx
	addl	$131071, %edx
	
	.subsections_via_symbols
