    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movl $300, %eax
    cltq
    pushq %rax
    leaq -8(%rbp), %rax
    movq %rax, %rcx
    popq %rax
    movq %rax, (%rcx)
    movq -8(%rbp), %rax
    movl %eax, %eax
    pushq %rax
    leaq -12(%rbp), %rax
    movq %rax, %rcx
    popq %rax
    movl %eax, (%rcx)
    cltq
    movslq -12(%rbp), %rax
    pushq %rax
    leaq -14(%rbp), %rax
    movq %rax, %rcx
    popq %rax
    movw %ax, (%rcx)
    movswq %ax, %rax
    movswq -14(%rbp), %rax
    pushq %rax
    leaq -15(%rbp), %rax
    movq %rax, %rcx
    popq %rax
    movb %al, (%rcx)
    movsbq %al, %rax
    movsbq -15(%rbp), %rax
    movsbl %al, %eax
    jmp .L0
.L0:
    movq %rbp, %rsp
    popq %rbp
    ret
