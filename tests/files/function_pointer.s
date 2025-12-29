    .globl add
add:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movl %edi, -4(%rbp)
    movl %esi, -8(%rbp)
    movslq -4(%rbp), %rax
    pushq %rax
    movslq -8(%rbp), %rax
    popq %rcx
    addl %ecx, %eax
    cltq
    jmp .L0
.L0:
    movq %rbp, %rsp
    popq %rbp
    ret
    .globl multiply
multiply:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movl %edi, -4(%rbp)
    movl %esi, -8(%rbp)
    movslq -4(%rbp), %rax
    pushq %rax
    movslq -8(%rbp), %rax
    popq %rcx
    imull %ecx, %eax
    cltq
    jmp .L1
.L1:
    movq %rbp, %rsp
    popq %rbp
    ret
    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    leaq add(%rip), %rax
    movq (%rax), %rax
    movq %rax, %r11
    movl $3, %eax
    cltq
    pushq %rax
    movl $4, %eax
    cltq
    pushq %rax
    popq %rsi
    popq %rdi
    call *%r11
    movl %eax, -4(%rbp)
    leaq multiply(%rip), %rax
    movq (%rax), %rax
    movq %rax, %r11
    movl $5, %eax
    cltq
    pushq %rax
    movl $6, %eax
    cltq
    pushq %rax
    popq %rsi
    popq %rdi
    call *%r11
    movl %eax, -8(%rbp)
    movl $0, %eax
    cltq
    jmp .L2
.L2:
    movq %rbp, %rsp
    popq %rbp
    ret
