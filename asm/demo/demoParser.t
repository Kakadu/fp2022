$ ./demoParse.exe
[(Code                              
[(Id (Label "fibonachch"));
(Command (Args1 ((Mnemonic "PUSH"), (Reg "RBP"))));
(Command (Args2 ((Mnemonic "MOV"), (Reg "RBX"), (Const "0x2A"))));
(Command (Args2 ((Mnemonic "MOV"), (Reg "RAX"), (Const "0x0"))));
(Command (Args2 ((Mnemonic "MOV"), (Reg "RCX"), (Const "1"))));
(Command (Args2 ((Mnemonic "CMP"), (Reg "RBX"), (Const "1"))));
(Command (Args1 ((Mnemonic "JE"), (Lab (Label "fibonachchEnd")))));
(Command (Args2 ((Mnemonic "CMP"), (Reg "RBX"), (Const "2"))));
(Command (Args1 ((Mnemonic "JE"), (Lab (Label "fibonachchTwo")))));
(Command (Args2 ((Mnemonic "SUB"), (Reg "RBX"), (Const "1"))));
(Id (Label "fibonachchStart"));
(Command (Args2 ((Mnemonic "SUB"), (Reg "RBX"), (Const "1"))));
(Command (Args2 ((Mnemonic "XOR"), (Reg "RAX"), (Reg "RCX"))));
(Command (Args2 ((Mnemonic "XOR"), (Reg "RCX"), (Reg "RAX"))));
(Command (Args2 ((Mnemonic "XOR"), (Reg "RAX"), (Reg "RCX"))));
(Command (Args2 ((Mnemonic "ADD"), (Reg "RAX"), (Reg "RCX"))));
(Command (Args2 ((Mnemonic "CMP"), (Reg "RBX"), (Const "0"))));
(Command (Args1 ((Mnemonic "JE"), (Lab (Label "fibonachchEnd")))));
(Command (Args1 ((Mnemonic "JMP"), (Lab (Label "fibonachchStart")))));
(Id (Label "fibonachchTwo"));
(Command (Args2 ((Mnemonic "MOV"), (Reg "RAX"), (Const "1"))));
(Id (Label "fibonachchEnd"));
(Command (Args1 ((Mnemonic "POP"), (Reg "RBP"))));
(Command (Args0 (Mnemonic "RET")))])
]
