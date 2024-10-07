    .text
    .globl main
main:
    pushq %rbp

    movq $2, x
    movq x, %rax
    addq %rax, %rax
    movq %rax, y
    movq y, %rax
    movq x, %rbx
    addq %rbx, %rax
    movq $format, %rdi
    movq %rax, %rsi
    call printf

    movq $0, %rax
    popq %rbp
    ret
    
    .data
format:
    .string "%d\n"
x:
    .int 0
y:
    .int 0
    