    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $32, %rsp
    movl $42, %eax
    cltq
    pushq %rax
    leaq -4(%rbp), %rax
    movq %rax, %rcx
    popq %rax
    movl %eax, (%rcx)
    cltq
    movslq -4(%rbp), %rax
    movslq %eax, %rax
    pushq %rax
    leaq -16(%rbp), %rax
    movq %rax, %rcx
    popq %rax
    movq %rax, (%rcx)
    movq -16(%rbp), %rax
    pushq %rax
    leaq -17(%rbp), %rax
    movq %rax, %rcx
    popq %rax
    movb %al, (%rcx)
    movsbq %al, %rax
    movsbq -17(%rbp), %rax
    movsbl %al, %eax
    jmp .L0
.L0:
    movq %rbp, %rsp
    popq %rbp
    ret
