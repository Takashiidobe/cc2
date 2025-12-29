    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movl $0, %eax
    cltq
    movl %eax, -4(%rbp)
    jmp skip
    movl $99, %eax
    cltq
    pushq %rax
    leaq -4(%rbp), %rax
    movq %rax, %rcx
    popq %rax
    movl %eax, (%rcx)
    cltq
skip:
    movslq -4(%rbp), %rax
    jmp .L0
.L0:
    movq %rbp, %rsp
    popq %rbp
    ret
