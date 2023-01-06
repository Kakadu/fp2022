  $ ./demoAsm.exe <<- EOF
  > section .data
  > a db 1, 2, 3
  > b db 4, 5, 6
  > c db 0xFF
  > section .code
  > mov xmm0, a
  > mov xmm1, b
  > imul xmm0, xmm1
  > xor xmm1, xmm1
  > add xmm1, xmm0
  > and xmm1, c
  > shr xmm0, 8
  > add xmm1, xmm0
  > and xmm1, c
  > shr xmm0, 8
  > add xmm1, xmm0
  > and xmm1, c
  > shr xmm0, 8
  > EOF
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
   "c": (Const [255L]),
   ]
