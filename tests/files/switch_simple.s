    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movl $2, %eax
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
    je .L_case_2_3
    movq (%rsp), %rax
    cmpq $3, %rax
    je .L_case_2_6
    jmp .L1
.L_case_2_0:
    movl $10, %eax
    cltq
    pushq %rax
    leaq -8(%rbp), %rax
    movq %rax, %rcx
    popq %rax
    movl %eax, (%rcx)
    cltq
    jmp .L1
.L_case_2_3:
    movl $20, %eax
    cltq
    pushq %rax
    leaq -8(%rbp), %rax
    movq %rax, %rcx
    popq %rax
    movl %eax, (%rcx)
    cltq
    jmp .L1
.L_case_2_6:
    movl $30, %eax
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
