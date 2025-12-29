    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movl $55, %eax
    cltq
    movl %eax, -4(%rbp)
    leaq -4(%rbp), %rax
    movq %rax, -16(%rbp)
    movq -16(%rbp), %rax
    movslq (%rax), %rax
    jmp .L0
.L0:
    movq %rbp, %rsp
    popq %rbp
    ret
