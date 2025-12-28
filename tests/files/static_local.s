    .data
    .align 4
count:
    .long 0
    .text
    .globl counter
counter:
    pushq %rbp
    movq %rsp, %rbp
    movslq count(%rip), %rax
    pushq %rax
    movl $1, %eax
    cltq
    popq %rcx
    addl %ecx, %eax
    cltq
    pushq %rax
    leaq count(%rip), %rax
    movq %rax, %rcx
    popq %rax
    movl %eax, (%rcx)
    cltq
    movslq count(%rip), %rax
    jmp .L0
.L0:
    popq %rbp
    ret
    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    call counter
    call counter
    call counter
    pushq %rax
    leaq -4(%rbp), %rax
    movq %rax, %rcx
    popq %rax
    movl %eax, (%rcx)
    cltq
    movslq -4(%rbp), %rax
    jmp .L1
.L1:
    movq %rbp, %rsp
    popq %rbp
    ret
