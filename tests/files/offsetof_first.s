    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    movq $0, %rax
    jmp .L0
.L0:
    popq %rbp
    ret
