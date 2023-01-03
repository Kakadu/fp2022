(** Copyright 2021-2022, Startsev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

let is_ws = function ' ' | '\n' | '\t' | '\r' -> true | _ -> false
let whitespaces = skip_while is_ws
let trim x = whitespaces *> x <* whitespaces
let parens p = trim (char '(' *> p <* char ')')
let is_num = function '0' .. '9' -> true | _ -> false
let is_ch = function 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false

let is_hex_digit = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'Z' -> true
  | _ -> false

let is_8bitreg = function
  | "AH" | "AL" | "BH" | "BL" | "CH" | "CL" | "DH" | "DL" -> true
  | _ -> false

let is_16bitreg = function "AX" | "BX" | "CX" | "DX" -> true | _ -> false

let is_32bitreg = function
  | "EAX" | "EBX" | "ECX" | "EDX" | "ESI" | "EDI" | "ESP" | "EBP" -> true
  | _ -> false

let is_64bitreg = function
  | "RAX" | "RBX" | "RCX" | "RDX" | "RSP" | "RBP" | "RSI" | "RDI" -> true
  | _ -> false

let is_128bitreg = function
  | "XMM0" | "XMM1" | "XMM2" | "XMM3" | "XMM4" | "XMM5" | "XMM6" | "XMM7" ->
      true
  | _ -> false

let is_reg s =
  List.fold_left ( || ) false
    [
      is_8bitreg s; is_16bitreg s; is_32bitreg s; is_64bitreg s; is_128bitreg s;
    ]

let is_arg0 = function "RET" | "SYSCALL" -> true | _ -> false

let is_arg1 = function
  | "PUSH" | "POP" | "INC" | "DEC" | "NOT" | "NEG" | "JMP" | "JE" | "JNE" | "JZ"
  | "JG" | "JGE" | "JL" | "JLE" ->
      true
  | _ -> false

let is_arg2 = function
  | "MOV" | "ADD" | "SUB" | "IMUL" | "AND" | "XOR" | "OR" | "SHL" | "SHR"
  | "CMP" ->
      true
  | _ -> false

let is_data_dec = function
  | "DB" | "DW" | "DD" | "DQ" | "DT" -> true
  | _ -> false

let is_mnemonic s = is_arg0 s || is_arg1 s || is_arg2 s

(** parses integer like 42727 *)
let nums = take_while1 is_num

let hex_nums = take_while1 is_hex_digit

let num =
  let sign = option "" (string "+" <|> string "-") in
  let hex_pref = option "" (string "0x") in
  sign >>= fun s ->
  hex_pref >>= fun p ->
  (match p with "" -> nums | _ -> hex_nums) >>= fun n ->
  return @@ String.concat "" [ s; p; n ]

(** parses word of letters *)
let word = take_while1 is_ch

(** parses a register of one of bit size *)
let reg =
  trim @@ word >>= fun x ->
  match String.uppercase_ascii x with
  | w when is_8bitreg w -> return @@ Dyn (Reg8 w)
  | w when is_16bitreg w -> return @@ Dyn (Reg16 w)
  | w when is_32bitreg w -> return @@ Dyn (Reg32 w)
  | w when is_64bitreg w -> return @@ Dyn (Reg64 w)
  | w when is_128bitreg w -> return @@ Dyn (Reg128 w)
  | _ -> fail "Isnt reg"

(** parses arithmetic expression with vars *)
let expr =
  let add = char '+' *> return (fun x y -> Add (x, y)) in
  let sub = char '-' *> return (fun x y -> Sub (x, y)) in
  let mul = char '*' *> return (fun x y -> Mul (x, y)) in
  let div = char '/' *> return (fun x y -> Div (x, y)) in
  let num = num >>= fun x -> return @@ Const (ASMConst x) in
  let var =
    word >>= fun x ->
    let w = String.uppercase_ascii x in
    if (not (is_reg w)) & not (is_mnemonic w) then return @@ Var (ASMVar x)
    else fail "Vars cant have name of regs and mnemonics"
  in
  let arg = num <|> var in
  let chainl1 e op =
    let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
    e >>= fun init -> go init
  in
  fix (fun expr ->
      let factor = parens expr <|> trim arg in
      let term = trim @@ chainl1 factor (mul <|> div) in
      trim @@ chainl1 term (add <|> sub))

(** parses label*)
let label =
  word >>= fun x ->
  let w = String.uppercase_ascii x in
  if (not (is_reg w)) & not (is_mnemonic w) then return @@ ASMLabel x
  else fail "Label cant have name of regs and mnemonics"

(** parses register with expression separeted by , *)
let rte =
  reg >>= fun (Dyn x) ->
  trim @@ (char ',' *> expr) >>= fun y -> return @@ RegToExpr (x, y)

(** parses two registers of the same bit size separeted br , *)
let rtr =
  let rr (Dyn x) (Dyn y) =
    let matching x y = RegToReg (x, y) in
    match (x, y) with
    | Reg8 _, Reg8 _ -> return @@ matching x y
    | Reg16 _, Reg16 _ -> return @@ matching x y
    | Reg32 _, Reg32 _ -> return @@ matching x y
    | Reg64 _, Reg64 _ -> return @@ matching x y
    | Reg128 _, Reg128 _ -> return @@ matching x y
    | _ -> fail "Isnt same type regs"
  in
  reg >>= fun x ->
  trim @@ (char ',' *> reg) >>= fun y -> rr x y

let da = rtr <|> rte

(** parses mnemonic with arguments *)
let command =
  trim word >>= fun x ->
  let w = String.uppercase_ascii x in
  match w with
  | "RET" -> return RET
  | "SYSCALL" -> return SYSCALL
  | "PUSH" -> reg >>= fun (Dyn x) -> return @@ PUSH x
  | "POP" -> reg >>= fun (Dyn x) -> return @@ POP x
  | "INC" -> reg >>= fun (Dyn x) -> return @@ INC x
  | "DEC" -> reg >>= fun (Dyn x) -> return @@ DEC x
  | "NOT" -> reg >>= fun (Dyn x) -> return @@ NOT x
  | "NEG" -> reg >>= fun (Dyn x) -> return @@ NEG x
  | "JMP" -> label >>= fun x -> return @@ JMP x
  | "JE" -> label >>= fun x -> return @@ JE x
  | "JNE" -> label >>= fun x -> return @@ JNE x
  | "JZ" -> label >>= fun x -> return @@ JZ x
  | "JG" -> label >>= fun x -> return @@ JG x
  | "JGE" -> label >>= fun x -> return @@ JGE x
  | "JL" -> label >>= fun x -> return @@ JL x
  | "JLE" -> label >>= fun x -> return @@ JLE x
  | "MOV" -> da >>= fun x -> return @@ MOV x
  | "ADD" -> da >>= fun x -> return @@ ADD x
  | "SUB" -> da >>= fun x -> return @@ SUB x
  | "IMUL" -> da >>= fun x -> return @@ IMUL x
  | "AND" -> da >>= fun x -> return @@ AND x
  | "XOR" -> da >>= fun x -> return @@ XOR x
  | "OR" -> da >>= fun x -> return @@ OR x
  | "SHL" ->
      reg >>= fun (Dyn x) ->
      (trim @@ char ',') *> expr >>= fun y -> return @@ SHL (x, y)
  | "SHR" ->
      reg >>= fun (Dyn x) ->
      (trim @@ char ',') *> expr >>= fun y -> return @@ SHR (x, y)
  | "CMP" -> da >>= fun x -> return @@ CMP x
  | _ -> fail "Isnt mnemonic"

(** parses one line of code: mnemonic and her argumets or label then return Ast.code_section *)
let code_line_parser =
  let label = label <* char ':' >>= fun x -> return @@ Id x in
  (* let coms = char ';' *> skip_while (fun x -> compare x '\n' != 0) in *)
  let cmd = command >>= fun x -> return @@ Command x in
  trim @@ cmd <|> label

(** parses one line of data section: type of const and const then return Ast.var t *)
let data_line_parser =
  let dt =
    trim word >>= fun x ->
    let w = String.uppercase_ascii x in
    match w with
    | "DB" -> return @@ DB
    | "DW" -> return @@ DW
    | "DD" -> return @@ DD
    | "DQ" -> return @@ DQ
    | "DT" -> return @@ DT
    | _ -> fail "Isnt fata type"
  in
  let var =
    trim word >>= fun x ->
    let w = String.uppercase_ascii x in
    if (not (is_reg w)) & not (is_mnemonic w) then return x
    else fail "Var's name must not be equal the name of reg or datatype"
  in
  let sep = trim @@ char ',' in
  var >>= fun v ->
  dt >>= fun dt ->
  word
  >>= (fun s -> return @@ Str s)
  <|> (sep_by sep num >>= fun n -> return @@ Num n)
  >>= fun l -> return @@ Variable (v, dt, l)

(** parses one of two possible sections and then parse section then return Ast.ast *)
let sec_parser =
  trim @@ (string "section" *> whitespaces *> char '.' *> word) >>= function
  | "code" | "text" ->
      many code_line_parser >>= fun values -> return (Code values)
  | "data" -> many data_line_parser >>= fun values -> return (Data values)
  | _ -> fail "Invalid section"

(** main paresr *)
let parser = many sec_parser >>= fun x -> return @@ Ast x

let parse = parse_string ~consume:All parser

(** results of parse *)
type 'a parse_rez = Parsed of 'a | Failed of string

(** main main parser *)
let eval str = match parse str with Ok v -> Parsed v | Error msg -> Failed msg

(*******************************************tests*******************************************)
let test_p p str expr =
  match parse_string ~consume:All p str with
  | Ok v -> v = expr
  | Error _ -> false

let pr_opt p str = Result.get_ok @@ parse_string ~consume:All p str
let pr_not_opt p str = Result.get_error @@ parse_string ~consume:All p str

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr "0");
  [%expect {|(Const (ASMConst "0"))|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr "0xa");
  [%expect {|(Const (ASMConst "0xa"))|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr "    0xa   ");
  [%expect {|(Const (ASMConst "0xa"))|}]

let%expect_test _ =
  print_string (pr_not_opt expr "rax");
  [%expect {| : Vars cant have name of regs and mnemonics |}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr "var");
  [%expect {| (Var (ASMVar "var")) |}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr "vAr");
  [%expect {| (Var (ASMVar "vAr")) |}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr "labl");
  [%expect {|(Var (ASMVar "labl"))|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr "lAbl");
  [%expect {|(Var (ASMVar "lAbl"))|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr "0 + 0x0");
  [%expect {|(Add ((Const (ASMConst "0")), (Const (ASMConst "0x0"))))|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr "0 + var");
  [%expect {| (Add ((Const (ASMConst "0")), (Var (ASMVar "var")))) |}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr "0+0x0");
  [%expect {|(Add ((Const (ASMConst "0")), (Const (ASMConst "0x0"))))|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr "0      +     0x0");
  [%expect {|(Add ((Const (ASMConst "0")), (Const (ASMConst "0x0"))))|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr "0 + (0x0)");
  [%expect {|(Add ((Const (ASMConst "0")), (Const (ASMConst "0x0"))))|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr "0 + (    0x0   )   ");
  [%expect {|(Add ((Const (ASMConst "0")), (Const (ASMConst "0x0"))))|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr "(0 + (0x0))");
  [%expect {|(Add ((Const (ASMConst "0")), (Const (ASMConst "0x0"))))|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr "0 + 1 * 2");
  [%expect
    {|(Add ((Const (ASMConst "0")), (Mul ((Const (ASMConst "1")), (Const (ASMConst "2"))))))|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr "0 * 1 + 2");
  [%expect
    {|(Add ((Mul ((Const (ASMConst "0")), (Const (ASMConst "1")))), (Const (ASMConst "2"))))|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr "0 * (1 + 2)");
  [%expect
    {|(Mul ((Const (ASMConst "0")), (Add ((Const (ASMConst "1")), (Const (ASMConst "2"))))))|}]

let%expect_test _ =
  print_string @@ show_code_section (pr_opt code_line_parser "ret");
  [%expect {|(Command (RET))|}]

let%expect_test _ =
  print_string @@ show_code_section (pr_opt code_line_parser "rEt");
  [%expect {|(Command (RET))|}]

let%expect_test _ =
  print_string
  @@ show_code_section (pr_opt code_line_parser "       \nret    \n\n");
  [%expect {|(Command (RET))|}]

let%expect_test _ =
  print_string @@ show_code_section (pr_opt code_line_parser "inc rax");
  [%expect {|(Command (INC (Reg64 "RAX")))|}]

let%expect_test _ =
  print_string
  @@ show_code_section
       (pr_opt code_line_parser "    inc                  rax           ");
  [%expect {|(Command (INC (Reg64 "RAX")))|}]

let%expect_test _ =
  print_string @@ show_code_section (pr_opt code_line_parser "mov rax, 1");
  [%expect
    {| (Command (MOV (RegToExpr ((Reg64 "RAX"), (Const (ASMConst "1")))))) |}]

let%expect_test _ =
  print_string
  @@ show_code_section (pr_opt code_line_parser "mov rax,         1        ");
  [%expect
    {| (Command (MOV (RegToExpr ((Reg64 "RAX"), (Const (ASMConst "1")))))) |}]

let%expect_test _ =
  print_string @@ show_code_section (pr_opt code_line_parser "mov \nrax, \n1\n");
  [%expect
    {| (Command (MOV (RegToExpr ((Reg64 "RAX"), (Const (ASMConst "1")))))) |}]

let%expect_test _ =
  print_string @@ show_code_section (pr_opt code_line_parser "mov rax, 1 + 1");
  [%expect
    {| (Command (MOV (RegToExpr ((Reg64 "RAX"), (Add ((Const (ASMConst "1")), (Const (ASMConst "1")))))))) |}]

let%expect_test _ =
  print_string @@ show_code_section (pr_opt code_line_parser "mov rax, rax");
  [%expect {| (Command (MOV (RegToReg ((Reg64 "RAX"), (Reg64 "RAX"))))) |}]

let%expect_test _ =
  print_string @@ show_code_section (pr_opt code_line_parser "mov rax, rbx");
  [%expect {| (Command (MOV (RegToReg ((Reg64 "RAX"), (Reg64 "RBX"))))) |}]

let%expect_test _ =
  print_string (pr_not_opt code_line_parser "mov rax, eax");
  [%expect {| : Label cant have name of regs and mnemonics |}]

let%expect_test _ =
  print_string (pr_not_opt code_line_parser "mov rax, ebx");
  [%expect {| : Label cant have name of regs and mnemonics |}]

let%expect_test _ =
  print_string @@ show_code_section (pr_opt code_line_parser "shl rax, 1");
  [%expect
    {| (Command (SHL (RegToExpr ((Reg64 "RAX"), (Const (ASMConst "1")))))) |}]

let%expect_test _ =
  print_string (pr_not_opt code_line_parser "shl rax, rax");
  [%expect {| : Label cant have name of regs and mnemonics |}]

let%expect_test _ =
  print_string @@ show_var (pr_opt data_line_parser "a dd 1");
  [%expect {| (Variable ("a", DD, (Num ["1"]))) |}]

let%expect_test _ =
  print_string @@ show_var (pr_opt data_line_parser "a dd 1, 2");
  [%expect {| (Variable ("a", DD, (Num ["1"; "2"]))) |}]

let%expect_test _ =
  print_string @@ show_var (pr_opt data_line_parser "a   dd    1   ,     2");
  [%expect {| (Variable ("a", DD, (Num ["1"; "2"]))) |}]

let%expect_test _ =
  print_string @@ show_var (pr_opt data_line_parser "a   dd aaa");
  [%expect{| (Variable ("a", DD, (Str "aaa"))) |}]

let%expect_test _ =
  print_string (pr_not_opt data_line_parser "a   dd aaa, aaaa");
  [%expect {| : end_of_input |}]

let%expect_test _ =
  print_string @@ show_var (pr_opt data_line_parser "a dD 1");
  [%expect {| (Variable ("a", DD, (Num ["1"]))) |}]

let%expect_test _ =
  print_string @@ show_var (pr_opt data_line_parser "A dd 1");
  [%expect {| (Variable ("A", DD, (Num ["1"]))) |}]

let%expect_test _ =
  print_string @@ show_dir (pr_opt sec_parser "section .code");
  [%expect {|
    Code [
    ]|}]

let%expect_test _ =
  print_string @@ show_dir (pr_opt sec_parser "section .text");
  [%expect {|
    Code [
    ]|}]

let%expect_test _ =
  print_string @@ show_dir (pr_opt sec_parser "section .data");
  [%expect {|
    Data [
    ]|}]

let%expect_test _ =
  print_string @@ show_dir (pr_opt sec_parser "section .code ret");
  [%expect {|
    Code [
    		(Command (RET));
    ]|}]

let%expect_test _ =
  print_string @@ show_dir (pr_opt sec_parser "section .code   ret   ");
  [%expect {|
    Code [
    		(Command (RET));
    ]|}]

let%expect_test _ =
  print_string @@ show_dir (pr_opt sec_parser "section .code inc rax inc rax");
  [%expect
    {|
         Code [
         		(Command (INC (Reg64 "RAX")));
         		(Command (INC (Reg64 "RAX")));
         ]|}]

let%expect_test _ =
  print_string @@ show_dir (pr_opt sec_parser "section .data a dd 1");
  [%expect {|
    Data [
    		(Variable ("a", DD, (Num ["1"])))
    ] |}]

let%expect_test _ =
  print_string @@ show_dir (pr_opt sec_parser "section .data a dd 1 b dd 2");
  [%expect
    {|
    Data [
    		(Variable ("a", DD, (Num ["1"])))
    		(Variable ("b", DD, (Num ["2"])))
    ] |}]

let%expect_test _ =
  print_string
  @@ show_ast (pr_opt parser "section .data a dd 1 section .text ret");
  [%expect
    {|
    (Ast [
    	Data [
    		(Variable ("a", DD, (Num ["1"])))
    ];
    	Code [
    		(Command (RET));
    ];
    ] |}]

let%expect_test _ =
  print_string
  @@ show_ast
       (pr_opt parser "section .data a dd 1 section .data section .text ret");
  [%expect
    {|
    (Ast [
    	Data [
    		(Variable ("a", DD, (Num ["1"])))
    ];
    	Data [
    ];
    	Code [
    		(Command (RET));
    ];
    ] |}]
