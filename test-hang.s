	.text
	.file	"test-hang.ll"
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp0:
	.cfi_def_cfa_offset 16
.Ltmp1:
	.cfi_offset %rbx, -16
	movl	$8, %edi
	callq	malloc
	movq	%rax, %rbx
	movq	(%rbx), %rax
	movl	$0, (%rax)
	movq	(%rbx), %rax
	movl	$1, 4(%rax)
	movq	(%rbx), %rax
	movl	(%rax), %esi
	movl	$.Lfmt, %edi
	xorl	%eax, %eax
	callq	printf
	movq	(%rbx), %rax
	movl	4(%rax), %esi
	movl	$.Lfmt.1, %edi
	xorl	%eax, %eax
	callq	printf
	xorl	%eax, %eax
	popq	%rbx
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc

	.type	.Lfmt,@object           # @fmt
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lfmt:
	.asciz	"%d\n"
	.size	.Lfmt, 4

	.type	.Lfmt.1,@object         # @fmt.1
.Lfmt.1:
	.asciz	"%d\n"
	.size	.Lfmt.1, 4


	.section	".note.GNU-stack","",@progbits
