    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movl $17, %eax
    cltq
    movl %eax, -4(%rbp)
    movslq -4(%rbp), %rax
    jmp .L0
.L0:
    movq %rbp, %rsp
    popq %rbp
    ret
