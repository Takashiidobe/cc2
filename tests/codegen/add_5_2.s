    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    movq $5, %rax
    pushq %rax
    movq $2, %rax
    popq %rcx
    addq %rcx, %rax
    popq %rbp
    ret
    popq %rbp
    ret
