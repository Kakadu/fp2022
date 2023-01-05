$ ./demoAsm.exe <<- EOF
>section .code
>fibonachch:
>push rbp
>mov rbx, 0x2A
>mov rax, 0x0
>mov rcx, 1
>cmp rbx, 1
>je fibonachchEnd
>cmp rbx, 2
>je fibonachchTwo
>sub rbx, 1
>fibonachchStart:
>sub rbx, 1
>xor rax, rcx
>xor rcx, rax
>xor rax, rcx
>add rax, rcx
>cmp rbx, 0
>je fibonachchEnd
>jmp fibonachchStart
>fibonachchTwo:
>mov rax, 1
>fibonachchEnd:
>pop rbp
>ret
>EOF
["OF": (Flag false),                
"RAX": (Reg64 0L),
"RBP": (Reg64 0L),
"RBX": (Reg64 0L),
"RCX": (Reg64 0L),
"RDI": (Reg64 0L),
"RDX": (Reg64 0L),
"RSI": (Reg64 0L),
"RSP": (Reg64 0L),
"SF": (Flag false),
"XMM0": (Reg128 (0L, 0L)),
"XMM1": (Reg128 (0L, 32L)),
"XMM2": (Reg128 (0L, 0L)),
"XMM3": (Reg128 (0L, 0L)),
"XMM4": (Reg128 (0L, 0L)),
"XMM5": (Reg128 (0L, 0L)),
"XMM6": (Reg128 (0L, 0L)),
"XMM7": (Reg128 (0L, 0L)),
"ZF": (Flag false),
"a": (Const [1L; 2L; 3L]),
"b": (Const [4L; 5L; 6L]),
"c": (Const [255L],
]
$ cd asm && make
0000000009DE8D6D
