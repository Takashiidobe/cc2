    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    movq $4, %rax
    jmp .L0
.L0:
    popq %rbp
    ret
