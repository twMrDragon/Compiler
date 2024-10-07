    .text
    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $24, %rsp

    movq $3, (%rsp)
    movq (%rsp), %rax
    imulq %rax, %rax
    movq $format, %rdi
    movq %rax, %rsi
    subq $8, %rsp
    call printf
    addq $8, %rsp

    movq $3, (%rsp)
    movq (%rsp),%rax
    movq %rax, %rbx
    addq %rbx, %rbx
    movq %rbx,8(%rsp)
    mov 8(%rsp), %rbx
    imulq %rax, %rbx
    addq $3, %rax
    movq %rax, 16(%rsp)
    movq 16(%rsp), %rcx
    movq %rcx, %rax
    idivq %rcx
    movq %rax, %rcx
    addq %rcx, %rbx
    movq $format, %rdi
    movq %rbx, %rsi
    subq $8, %rsp
    call printf
    addq $8, %rsp

    addq $24, %rsp
    movq $0, %rax
    movq %rbp, %rsp
    popq %rbp
    ret
    
    .data
format:
    .string "%d\n"
    