  $ ./demoParse.exe <<- EOF
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
  (Ast [
  	Code [
  		(Id (ASMLabel "fibonachch"));
  		(Command (PUSH (Reg64 "RBP")));
  		(Command (MOV (RegToExpr ((Reg64 "RBX"), (Const (ASMConst "0x2A"))))));
  		(Command (MOV (RegToExpr ((Reg64 "RAX"), (Const (ASMConst "0x0"))))));
  		(Command (MOV (RegToExpr ((Reg64 "RCX"), (Const (ASMConst "1"))))));
  		(Command (CMP (RegToExpr ((Reg64 "RBX"), (Const (ASMConst "1"))))));
  		(Command (JE (ASMLabel "fibonachchEnd")));
  		(Command (CMP (RegToExpr ((Reg64 "RBX"), (Const (ASMConst "2"))))));
  		(Command (JE (ASMLabel "fibonachchTwo")));
  		(Command (SUB (RegToExpr ((Reg64 "RBX"), (Const (ASMConst "1"))))));
  		(Id (ASMLabel "fibonachchStart"));
  		(Command (SUB (RegToExpr ((Reg64 "RBX"), (Const (ASMConst "1"))))));
  		(Command (XOR (RegToReg ((Reg64 "RAX"), (Reg64 "RCX")))));
  		(Command (XOR (RegToReg ((Reg64 "RCX"), (Reg64 "RAX")))));
  		(Command (XOR (RegToReg ((Reg64 "RAX"), (Reg64 "RCX")))));
  		(Command (ADD (RegToReg ((Reg64 "RAX"), (Reg64 "RCX")))));
  		(Command (CMP (RegToExpr ((Reg64 "RBX"), (Const (ASMConst "0"))))));
  		(Command (JE (ASMLabel "fibonachchEnd")));
  		(Command (JMP (ASMLabel "fibonachchStart")));
  		(Id (ASMLabel "fibonachchTwo"));
  		(Command (MOV (RegToExpr ((Reg64 "RAX"), (Const (ASMConst "1"))))));
  		(Id (ASMLabel "fibonachchEnd"));
  		(Command (POP (Reg64 "RBP")));
  		(Command (RET));
  ];
  ]
