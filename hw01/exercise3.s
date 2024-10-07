    .text
    .globl main
main:
    pushq %rbp

    movq $1, %rax
    andq $0, %rax
    movq $true, %rdi
    cmpq $0, %rax
    jne conditional1
    movq $false, %rdi
conditional1:
    call puts

    movq $10, %rsi
    imulq $2, %rsi
    movq $3, %rax
    cmpq $4, %rax
    jne conditional2
    movq $14, %rsi
conditional2:
    movq $format, %rdi
    call printf

    movq $2, %rax
    cmpq $3, %rax
    sete %al
    movq $2, %rbx
    imulq $3, %rbx
    cmpq $4, %rbx
    setnl %bl
    or %al, %bl
    movq $true, %rdi
    cmpq $0, %rbx
    jne conditional3
    movq $false, %rdi
conditional3:
    call puts
    
    movq $0, %rax
    popq %rbp
    ret

    .data
format:
    .string "%d\n"
true:
    .string "true"
false:
    .string "false"
    