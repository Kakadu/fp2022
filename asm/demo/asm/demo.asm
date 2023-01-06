 global _start
_start:


section .text
.fibonachch:
    push rbp
    mov rbx, 0x2A
    mov rax, 0x0
    mov rcx, 1
    cmp rbx, 1
je .fibonachchEnd
    cmp rbx, 2
je .fibonachchTwo
    sub rbx, 1
.fibonachchStart:
    sub rbx, 1
    xor rax, rcx
    xor rcx, rax
    xor rax, rcx
    add rax, rcx
    cmp rbx, 0
je .fibonachchEnd
    jmp .fibonachchStart
.fibonachchTwo:
    mov rax, 1
.fibonachchEnd:
    pop rbp


; Вывод регистра rax 
; 0x9DE8D6D -> 165580141
section .data
    codes db '0123456789ABCDEF'

section .text
    mov rdi, 1
    mov rdx, 1
    mov rcx, 64
.loop push rax
    sub rcx, 4
    sar rax, cl
    and rax, 0xf
    lea rsi, [codes + rax]
    mov rax, 1
    push rcx
    syscall
    pop rcx
    pop rax
    test rcx, rcx
jnz .loop
    mov     rax, 60  ; exit syscall
    xor     rdi, rdi ; ret code 0
    syscall 