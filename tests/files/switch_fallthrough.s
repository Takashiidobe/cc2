    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movl $1, %eax
    cltq
    movl %eax, -4(%rbp)
    movl $0, %eax
    cltq
    movl %eax, -8(%rbp)
    movslq -4(%rbp), %rax
    pushq %rax
    movq (%rsp), %rax
    cmpq $1, %rax
    je .L_case_2_0
    movq (%rsp), %rax
    cmpq $2, %rax
    je .L_case_2_2
    movq (%rsp), %rax
    cmpq $3, %rax
    je .L_case_2_4
    jmp .L1
.L_case_2_0:
    movslq -8(%rbp), %rax
    pushq %rax
    movl $1, %eax
    cltq
    popq %rcx
    addl %ecx, %eax
    cltq
    pushq %rax
    leaq -8(%rbp), %rax
    movq %rax, %rcx
    popq %rax
    movl %eax, (%rcx)
    cltq
.L_case_2_2:
    movslq -8(%rbp), %rax
    pushq %rax
    movl $10, %eax
    cltq
    popq %rcx
    addl %ecx, %eax
    cltq
    pushq %rax
    leaq -8(%rbp), %rax
    movq %rax, %rcx
    popq %rax
    movl %eax, (%rcx)
    cltq
.L_case_2_4:
    movslq -8(%rbp), %rax
    pushq %rax
    movl $100, %eax
    cltq
    popq %rcx
    addl %ecx, %eax
    cltq
    pushq %rax
    leaq -8(%rbp), %rax
    movq %rax, %rcx
    popq %rax
    movl %eax, (%rcx)
    cltq
    jmp .L1
.L1:
    addq $8, %rsp
    movslq -8(%rbp), %rax
    jmp .L0
.L0:
    movq %rbp, %rsp
    popq %rbp
    ret
