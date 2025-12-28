    .data
    .align 4
internal_var:
    .long 100
    .text
    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    movslq internal_var(%rip), %rax
    jmp .L0
.L0:
    popq %rbp
    ret
