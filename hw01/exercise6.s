    .text
    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $8, %rsp

    movq $0, (%rsp)
for_loop:
    cmpq $20, (%rsp)
    jg end_for_loop
    movq (%rsp), %rdi
    call isqrt
    movq %rax,%rdx
    movq $format, %rdi
    movq (%rsp), %rsi
    subq $8, %rsp
    call printf
    addq $8, %rsp
    incq (%rsp)
    jmp for_loop
end_for_loop:

    addq $8, %rsp
    movq $0, %rax
    movq %rbp, %rsp
    popq %rbp
    ret
isqrt:
    pushq %rbp
    movq %rsp, %rbp
    subq $24, %rsp

    movq %rdi, (%rsp) # n
    movq $0, 8(%rsp) # c
    movq $1, 16(%rsp) # s
while_loop:
    movq 16(%rsp), %rax
    cmp (%rsp), %rax
    jg end_while_loop
    incq 8(%rsp)
    movq 8(%rsp), %rbx
    leaq 1(%rax,%rbx,2), %rax
    movq %rax, 16(%rsp)
    jmp while_loop
end_while_loop:
    mov 8(%rsp), %rax

    addq $24, %rsp
    movq %rbp, %rsp
    popq %rbp
    ret
    .data
format:
    .string "sqrt(%2d) = %2d\n"
    