    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $32, %rsp
    movl $123, %eax
    cltq
    pushq %rax
    leaq -4(%rbp), %rax
    movq %rax, %rcx
    popq %rax
    movl %eax, (%rcx)
    cltq
    leaq -4(%rbp), %rax
    pushq %rax
    leaq -16(%rbp), %rax
    movq %rax, %rcx
    popq %rax
    movq %rax, (%rcx)
    movq -16(%rbp), %rax
    pushq %rax
    leaq -24(%rbp), %rax
    movq %rax, %rcx
    popq %rax
    movq %rax, (%rcx)
    movq -24(%rbp), %rax
    pushq %rax
    leaq -32(%rbp), %rax
    movq %rax, %rcx
    popq %rax
    movq %rax, (%rcx)
    movq -32(%rbp), %rax
    movslq (%rax), %rax
    jmp .L0
.L0:
    movq %rbp, %rsp
    popq %rbp
    ret
