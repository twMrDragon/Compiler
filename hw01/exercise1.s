    .text
    .globl main
main:
    pushq %rbp

    movq $format, %rdi
    movq $42, %rsi
    call printf

    movq $0, %rax
    popq %rbp
    ret

    .data
format:
    .string "n = %d\n"
    