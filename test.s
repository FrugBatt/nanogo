	.text
	.globl	main
main:
	call F_main
	xorq %rax, %rax
	ret
F_foo:
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	movq 16(%rbp), %rdi
	addq $0, %rdi
	pushq %rdi
	movq 16(%rbp), %rdi
	movq 0(%rdi), %rdi
	pushq %rdi
	movq $1, %rdi
	popq %r12
	addq %rdi, %r12
	movq %r12, %rdi
	popq %rax
	movq %rdi, 0(%rax)
	movq 16(%rbp), %rdi
	addq $8, %rdi
	pushq %rdi
	movq 16(%rbp), %rdi
	movq 8(%rdi), %rdi
	pushq %rdi
	movq $1, %rdi
	popq %r12
	addq %rdi, %r12
	movq %r12, %rdi
	popq %rax
	movq %rdi, 0(%rax)
	movq %rdi, %rax
E_foo:
	movq %rbp, %rsp
	popq %rbp
	ret
F_bar:
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	movq 16(%rbp), %rdi
	addq $0, %rdi
	pushq %rdi
	movq 16(%rbp), %rdi
	movq 0(%rdi), %rdi
	pushq %rdi
	movq $1, %rdi
	popq %r12
	addq %rdi, %r12
	movq %r12, %rdi
	popq %rax
	movq %rdi, 0(%rax)
	movq 16(%rbp), %rdi
	addq $8, %rdi
	pushq %rdi
	movq 16(%rbp), %rdi
	movq 8(%rdi), %rdi
	pushq %rdi
	movq $1, %rdi
	popq %r12
	addq %rdi, %r12
	movq %r12, %rdi
	popq %rax
	movq %rdi, 0(%rax)
	movq %rdi, %rax
E_bar:
	movq %rbp, %rsp
	popq %rbp
	ret
F_main:
	pushq %rbp
	movq %rsp, %rbp
	subq $8, %rsp
	movq $16, %rdi
	call allocz
	movq %rax, %rdi
	movq %rdi, -8(%rbp)
	movq -8(%rbp), %rdi
	addq $0, %rdi
	pushq %rdi
	movq $1, %rdi
	popq %rax
	movq %rdi, 0(%rax)
	movq -8(%rbp), %rdi
	addq $8, %rdi
	pushq %rdi
	movq $2, %rdi
	popq %rax
	movq %rdi, 0(%rax)
	movq -8(%rbp), %rdi
	movq 0(%rdi), %rdi
	call print_int
	call print_space
	movq -8(%rbp), %rdi
	movq 8(%rdi), %rdi
	call print_int
	movq $S_3, %rdi
	call print_string
	movq -8(%rbp), %rdi
	movq 0(%rdi), %rdi
	pushq %rdi
	call F_foo
	addq $8, %rsp
	movq %rax, %rdi
	movq -8(%rbp), %rdi
	movq 0(%rdi), %rdi
	call print_int
	call print_space
	movq -8(%rbp), %rdi
	movq 8(%rdi), %rdi
	call print_int
	movq $S_2, %rdi
	call print_string
	movq -8(%rbp), %rdi
	pushq %rdi
	call F_bar
	addq $8, %rsp
	movq %rax, %rdi
	movq -8(%rbp), %rdi
	movq 0(%rdi), %rdi
	call print_int
	call print_space
	movq -8(%rbp), %rdi
	movq 8(%rdi), %rdi
	call print_int
	movq $S_1, %rdi
	call print_string
	movq %rdi, %rax
E_main:
	movq %rbp, %rsp
	popq %rbp
	ret

print_int_or_nil:
        test    %rdi, %rdi
        jz      print_nil
        movq    (%rdi), %rdi
print_int:
        movq    %rdi, %rsi
        movq    $S_int, %rdi
        xorq    %rax, %rax
        call    printf
        ret
print_string:
        test    %rdi, %rdi
        jz      print_nil
        mov     %rdi, %rsi
        mov     $S_string, %rdi
        xorq    %rax, %rax
        call    printf
        ret
print_nil:
        mov     $S_nil, %rdi
        xorq    %rax, %rax
        call    printf
        ret
print_space:
        mov     $S_space, %rdi
        xorq    %rax, %rax
        call    printf
        ret
print_bool:
        xorq    %rax, %rax
        test    %rdi, %rdi
        jz      1f
        mov     $S_true, %rdi
        call    printf
        ret
1:      mov     $S_false, %rdi
        call    printf
        ret
allocz:
        movq    %rdi, %rbx     # callee-save
        call    malloc
        testq   %rbx, %rbx
        jnz     1f
        ret
1:      movb    $0, (%rax, %rbx)
        decq    %rbx
        jnz     1b
        ret
	.data
S_int:
	.string "%ld"
S_string:
	.string "%s"
S_true:
	.string "true"
S_false:
	.string "false"
S_nil:
	.string "<nil>"
S_space:
	.string " "
S_empty:
	.string ""
S_2:
	.string "\n"
S_3:
	.string "\n"
S_1:
	.string "\n"
