    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    movq $0, %rax
    pushq %rax
    movq $0, %rax
    popq %rcx
    addq %rcx, %rax
    jmp .L0
.L0:
    popq %rbp
    ret
