    .bss
    .align 4
    .globl global_value
global_value:
    .zero 4
    .text
    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    movl $55, %eax
    cltq
    pushq %rax
    leaq global_value(%rip), %rax
    movq %rax, %rcx
    popq %rax
    movl %eax, (%rcx)
    cltq
    movslq global_value(%rip), %rax
    jmp .L0
.L0:
    popq %rbp
    ret
