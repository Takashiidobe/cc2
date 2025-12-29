    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movl $0, %eax
    cltq
    movl %eax, -4(%rbp)
loop:
    movslq -4(%rbp), %rax
    pushq %rax
    movl $1, %eax
    cltq
    popq %rcx
    addl %ecx, %eax
    cltq
    pushq %rax
    leaq -4(%rbp), %rax
    movq %rax, %rcx
    popq %rax
    movl %eax, (%rcx)
    cltq
    movslq -4(%rbp), %rax
    pushq %rax
    movl $5, %eax
    cltq
    popq %rcx
    cmpl %eax, %ecx
    setl %al
    movzbq %al, %rax
    cmpq $0, %rax
    je .L2
    jmp loop
.L2:
    movslq -4(%rbp), %rax
    jmp .L0
.L0:
    movq %rbp, %rsp
    popq %rbp
    ret
