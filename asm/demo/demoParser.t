$ ./demoParse.exe
(Code                               
[(Id (Label "fibbonach"));
(Command (Args2 ((Mnemonic "MOV"), (Reg "RBX"), (Const "0x2A"))));
(Command (Args2 ((Mnemonic "MOV"), (Reg "RAX"), (Const "0x0"))));
(Command (Args2 ((Mnemonic "MOV"), (Reg "RCX"), (Const "1"))));
(Command (Args2 ((Mnemonic "CMP"), (Reg "RBX"), (Const "1"))));
(Command (Jmp ((Mnemonic "JE"), (Label "fibbonachEnd"))));
(Command (Args2 ((Mnemonic "CMP"), (Reg "RBX"), (Const "2"))));
(Command (Jmp ((Mnemonic "JE"), (Label "fibbonachTwo"))));
(Command (Args2 ((Mnemonic "SUB"), (Reg "RBX"), (Const "1"))));
(Id (Label "fibbonachStart"));
(Command (Args2 ((Mnemonic "SUB"), (Reg "RBX"), (Const "1"))));
(Command (Args2 ((Mnemonic "XOR"), (Reg "RAX"), (Reg "RCX"))));
(Command (Args2 ((Mnemonic "XOR"), (Reg "RCX"), (Reg "RAX"))));
(Command (Args2 ((Mnemonic "XOR"), (Reg "RAX"), (Reg "RCX"))));
(Command (Args2 ((Mnemonic "ADD"), (Reg "RAX"), (Reg "RCX"))));
(Command (Args2 ((Mnemonic "CMP"), (Reg "RBX"), (Const "0"))));
(Command (Jmp ((Mnemonic "JE"), (Label "fibbonachEnd"))));
(Command (Jmp ((Mnemonic "JMP"), (Label "fibbonachStart"))));
(Id (Label "fibbonachTwo"));
(Command (Args2 ((Mnemonic "MOV"), (Reg "RAX"), (Const "1"))));
(Id (Label "fibbonachEnd")); (Command (Args0 (Mnemonic "RET")))])
