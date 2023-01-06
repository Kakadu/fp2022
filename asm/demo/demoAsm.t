  $ ./demoAsm.exe <<- EOF
  > section .code
  > fibonachch:
  > push rbp
  > mov rbx, 0x2A
  > mov rax, 0x0
  > mov rcx, 1
  > cmp rbx, 1
  > je fibonachchEnd
  > cmp rbx, 2
  > je fibonachchTwo
  > sub rbx, 1
  > fibonachchStart:
  > sub rbx, 1
  > xor rax, rcx
  > xor rcx, rax
  > xor rax, rcx
  > add rax, rcx
  > cmp rbx, 0
  > je fibonachchEnd
  > jmp fibonachchStart
  > fibonachchTwo:
  > mov rax, 1
  > fibonachchEnd:
  > pop rbp
  > ret
  > EOF
  : end_of_input
  $ cd asm && make
  cd: asm: No such file or directory
  [1]
