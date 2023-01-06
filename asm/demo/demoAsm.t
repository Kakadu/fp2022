  $ ./demoAsm.exe <<-EOF
  > section .code
  > .fibonachch:
  > push rbp
  > mov rbx, 0x2A
  > mov rax, 0x0
  > mov rcx, 1
  > cmp rbx, 1
  > je .fibonachchEnd
  > cmp rbx, 2
  > je .fibonachchTwo
  > sub rbx, 1
  > .fibonachchStart:
  > sub rbx, 1
  > xor rax, rcx
  > xor rcx, rax
  > xor rax, rcx
  > add rax, rcx
  > cmp rbx, 0
  > je .fibonachchEnd
  > jmp .fibonachchStart
  > .fibonachchTwo:
  > mov rax, 1
  > .fibonachchEnd:
  > pop rbp
  > ret
  > EOF
  ["OF": (Flag false),
   "RAX": (Reg64 165580141L),
   "RBP": (Reg64 0L),
   "RBX": (Reg64 0L),
   "RCX": (Reg64 102334155L),
   "RDI": (Reg64 0L),
   "RDX": (Reg64 0L),
   "RSI": (Reg64 0L),
   "RSP": (Reg64 0L),
   "SF": (Flag false),
   "XMM0": (Reg128 (0L, 0L)),
   "XMM1": (Reg128 (0L, 0L)),
   "XMM2": (Reg128 (0L, 0L)),
   "XMM3": (Reg128 (0L, 0L)),
   "XMM4": (Reg128 (0L, 0L)),
   "XMM5": (Reg128 (0L, 0L)),
   "XMM6": (Reg128 (0L, 0L)),
   "XMM7": (Reg128 (0L, 0L)),
   "ZF": (Flag true),
   ]
  $ touch demo.asm
  $ tee demo.asm <<-EOF >> /dev/null
  > global _start
  > _start:
  > 
  > 
  > section .text
  > .fibonachch:
  >     push rbp
  >     mov rbx, 0x2A
  >     mov rax, 0x0
  >     mov rcx, 1
  >     cmp rbx, 1
  > je .fibonachchEnd
  >     cmp rbx, 2
  > je .fibonachchTwo
  >     sub rbx, 1
  > .fibonachchStart:
  >     sub rbx, 1
  >     xor rax, rcx
  >     xor rcx, rax
  >     xor rax, rcx
  >     add rax, rcx
  >     cmp rbx, 0
  > je .fibonachchEnd
  >     jmp .fibonachchStart
  > .fibonachchTwo:
  >     mov rax, 1
  > .fibonachchEnd:
  >     pop rbp
  > 
  > 
  > ; Вывод регистра rax 
  > ; 0x9DE8D6D -> 165580141
  > section .data
  >     codes db '0123456789ABCDEF'
  > 
  > section .text
  >     mov rdi, 1
  >     mov rdx, 1
  >     mov rcx, 64
  > .loop push rax
  >     sub rcx, 4
  >     sar rax, cl
  >     and rax, 0xf
  >     lea rsi, [codes + rax]
  >     mov rax, 1
  >     push rcx
  >     syscall
  >     pop rcx
  >     pop rax
  >     test rcx, rcx
  > jnz .loop
  >     mov     rax, 60  ; exit syscall
  >     xor     rdi, rdi ; ret code 0
  >     syscall 
  > EOF
  $ nasm -felf64 demo.asm -o demo.o && ld -o demo demo.o && ./demo
  0000000009DE8D6D
