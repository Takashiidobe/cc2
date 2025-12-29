    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movl $100, %eax
    cltq
    movq %rax, -8(%rbp)
    movq -8(%rbp), %rax
    movl %eax, %eax
    jmp .L0
.L0:
    movq %rbp, %rsp
    popq %rbp
    ret
