    .text
    .globl main
main:
    pushq %rbp

    movq $4, %rax
    addq $6, %rax
    movq $format, %rdi
    movq %rax, %rsi
    call printf

    movq $21, %rax
    imulq $2, %rax
    movq $format, %rdi
    movq %rax, %rsi
    call printf
    
    movl $7, %eax
    movl $2, %ecx
    idivl %ecx
    movq $format, %rdi
    movq %rax, %rsi
    addq $4, %rsi
    call printf

    movl $10, %eax
    movl $5, %ecx
    idivl %ecx
    imulq $6, %rax
    movq $3, %rsi
    subq %rax, %rsi
    movq $format, %rdi
    call printf

    movq $0, %rax
    popq %rbp
    ret

    .data
format:
    .string "%d\n"
    