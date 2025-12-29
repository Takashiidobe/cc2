    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movl $0, %eax
    cltq
    movl %eax, -4(%rbp)
    jmp second
first:
    movslq -4(%rbp), %rax
    pushq %rax
    movl $10, %eax
    cltq
    popq %rcx
    addl %ecx, %eax
    cltq
    pushq %rax
    leaq -4(%rbp), %rax
    movq %rax, %rcx
    popq %rax
    movl %eax, (%rcx)
    cltq
    jmp end
second:
    movslq -4(%rbp), %rax
    pushq %rax
    movl $5, %eax
    cltq
    popq %rcx
    addl %ecx, %eax
    cltq
    pushq %rax
    leaq -4(%rbp), %rax
    movq %rax, %rcx
    popq %rax
    movl %eax, (%rcx)
    cltq
    jmp first
end:
    movslq -4(%rbp), %rax
    jmp .L0
.L0:
    movq %rbp, %rsp
    popq %rbp
    ret
