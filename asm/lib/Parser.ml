(** Copyright 2021-2022, Startsev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

let is_ws = function ' ' | '\n' | '\t' | '\r' -> true | _ -> false
let whitespaces = skip_while is_ws
let trim x = whitespaces *> x <* whitespaces
let parens p = trim (char '(' *> p <* char ')')

let conde = function
  | [] -> fail "No conds in conde"
  | x :: xs -> List.fold_left ( <|> ) x xs

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
  | "PUSH" | "POP" | "INC" | "DEC" | "NOT" | "NEG" | "CALL" -> true
  | _ -> false

let is_arg2 = function
  | "MOV" | "ADD" | "SUB" | "IMUL" | "AND" | "XOR" | "OR" | "SHL" | "SHR"
  | "CMP" ->
      true
  | _ -> false

let is_jmp = function
  | "JMP" | "JE" | "JNE" | "JZ" | "JG" | "JGE" | "JL" | "JLE" -> true
  | _ -> false

let is_data_dec = function "DB" | "DW" | "DD" | "DQ" -> true | _ -> false
let is_mnemonic s = is_arg0 s || is_arg1 s || is_arg2 s || is_jmp s
let digit_c = satisfy is_num

(*parse digit and return int "5" -> 5 *)
let digit = digit_c >>= fun c -> return (Char.code c - Char.code '0')

(*parse integer like 42727*)
let nums = take_while1 is_num
let hex_nums = take_while1 is_hex_digit

(*parse word of letters*)
let word = take_while1 is_ch

(*parse expression that may content +, -, *, /, registers and int constants in hex and decimal forms then return Ast.expr where operations have prio*)
let expr_parser =
  let add = char '+' *> return (fun x y -> Add (x, y)) in
  let sub = char '-' *> return (fun x y -> Sub (x, y)) in
  let mul = char '*' *> return (fun x y -> Mul (x, y)) in
  let div = char '/' *> return (fun x y -> Div (x, y)) in
  let sign = option "" (string "+" <|> string "-") in
  let hex_prefix = option "" (string "0x") in
  let num =
    sign >>= fun s ->
    hex_prefix >>= fun p ->
    (match p with "" -> nums | _ -> hex_nums) >>= fun n ->
    return (Const (String.concat "" [ s; p; n ]))
  in
  let reg =
    word >>= fun x ->
    let w = String.uppercase_ascii x in
    if is_reg w then return (Reg w) else return (Var x)
  in
  let arg = reg <|> num in
  let chainl1 e op =
    let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
    e >>= fun init -> go init
  in
  fix (fun expr ->
      let factor = parens expr <|> trim arg in
      let term = trim @@ chainl1 factor (mul <|> div) in
      trim @@ chainl1 term (add <|> sub))

(*parse one line of code: mnemonic and her argumets or label then return Ast.code_section*)
let code_line_parser =
  let label = trim @@ word <* char ':' >>= fun x -> return (Id (Label x)) in
  let mnem =
    trim @@ word >>= fun x ->
    let w = String.uppercase_ascii x in
    if is_mnemonic w then return (Mnemonic w)
    else fail @@ "Invalid command " ^ x
  in
  let sep = trim @@ char ',' in
  let command (Mnemonic cmd) = cmd in
  let inst =
    mnem >>= fun cmd ->
    if is_jmp @@ command cmd then
      trim word >>= fun l -> return (Command (Jmp (cmd, Label l)))
    else
      trim @@ sep_by sep expr_parser >>= fun exprs ->
      match List.length exprs with
      | 0 when is_arg0 @@ command cmd -> return (Command (Args0 cmd))
      | 1 when is_arg1 @@ command cmd ->
          return (Command (Args1 (cmd, List.hd exprs)))
      | 2 when is_arg2 @@ command cmd ->
          return (Command (Args2 (cmd, List.hd exprs, List.nth exprs 1)))
      | _ -> fail "Invalid count of arguments"
  in
  label <|> inst

(*parse one line of data section: type of const and const then return Ast.var t*)
let data_line_parser =
  let data_t =
    trim word >>= fun x ->
    let w = String.uppercase_ascii x in
    if is_data_dec w then return (DataType w) else fail "Invalud datatype"
  in
  let var =
    trim word >>= fun x ->
    let w = String.uppercase_ascii x in
    if is_reg w || is_data_dec w then
      fail "Var's name must not be equal the name of reg or datatype"
    else return (fun dt y -> Variable (x, dt, y))
  in
  let sep = trim @@ char ',' in
  var >>= fun v ->
  data_t >>= fun dt ->
  trim @@ sep_by sep (word <|> nums) >>= fun l -> return (v dt l)

(*parse one of two possible sections and then parse section then return Ast.ast*)
let sec_parser =
  trim @@ (string "section" *> whitespaces *> char '.' *> word) >>= function
  | "code" | "text" ->
      many code_line_parser >>= fun values -> return (Code values)
  | "data" -> many data_line_parser >>= fun values -> return (Data values)
  | _ -> fail "Invalid section"

(*main paresr*)
let parser = many sec_parser
let parse = parse_string ~consume:All parser

(*main main parser*)
let eval str = match parse str with Ok v -> v | Error msg -> failwith msg

(*******************************************tests*******************************************)
let test_p p pp str =
  match parse_string ~consume:All p str with Ok v -> pp v | Error msg -> msg

let%test _ = test_p expr_parser show_expr "0" = "(Const \"0\")"
let%test _ = test_p expr_parser show_expr "0xa" = "(Const \"0xa\")"
let%test _ = test_p expr_parser show_expr "    0xa   " = "(Const \"0xa\")"
let%test _ = test_p expr_parser show_expr "rax" = "(Reg \"RAX\")"
let%test _ = test_p expr_parser show_expr "rAx" = "(Reg \"RAX\")"
let%test _ = test_p expr_parser show_expr "var" = "(Var \"var\")"
let%test _ = test_p expr_parser show_expr "vAr" = "(Var \"vAr\")"

let%test _ =
  test_p expr_parser show_expr "0 + 0x0"
  = "(Add ((Const \"0\"), (Const \"0x0\")))"

let%test _ =
  test_p expr_parser show_expr "0 + rax"
  = "(Add ((Const \"0\"), (Reg \"RAX\")))"

let%test _ =
  test_p expr_parser show_expr "0 + var"
  = "(Add ((Const \"0\"), (Var \"var\")))"

let%test _ =
  test_p expr_parser show_expr "0+0x0"
  = "(Add ((Const \"0\"), (Const \"0x0\")))"

let%test _ =
  test_p expr_parser show_expr "0      +     0x0"
  = "(Add ((Const \"0\"), (Const \"0x0\")))"

let%test _ =
  test_p expr_parser show_expr "0 + (0x0)"
  = "(Add ((Const \"0\"), (Const \"0x0\")))"

let%test _ =
  test_p expr_parser show_expr "0 + (    0x0   )   "
  = "(Add ((Const \"0\"), (Const \"0x0\")))"

let%test _ =
  test_p expr_parser show_expr "(0 + (0x0))"
  = "(Add ((Const \"0\"), (Const \"0x0\")))"

let%test _ =
  test_p expr_parser show_expr "0 + 1 * 2"
  = "(Add ((Const \"0\"), (Mul ((Const \"1\"), (Const \"2\")))))"

let%test _ =
  test_p expr_parser show_expr "0 * 1 + 2"
  = "(Add ((Mul ((Const \"0\"), (Const \"1\"))), (Const \"2\")))"

let%test _ =
  test_p expr_parser show_expr "0 * (1 + 2)"
  = "(Mul ((Const \"0\"), (Add ((Const \"1\"), (Const \"2\")))))"

let%test _ =
  test_p code_line_parser show_code_section "ret"
  = "(Command (Args0 (Mnemonic \"RET\")))"

let%test _ =
  test_p code_line_parser show_code_section "rEt"
  = "(Command (Args0 (Mnemonic \"RET\")))"

let%test _ =
  test_p code_line_parser show_code_section "       \nret    \n\n"
  = "(Command (Args0 (Mnemonic \"RET\")))"

let%test _ =
  test_p code_line_parser show_code_section "inc rax"
  = "(Command (Args1 ((Mnemonic \"INC\"), (Reg \"RAX\"))))"

let%test _ =
  test_p code_line_parser show_code_section
    "   inc              rax            "
  = "(Command (Args1 ((Mnemonic \"INC\"), (Reg \"RAX\"))))"

let%test _ =
  test_p code_line_parser show_code_section "inc rax + 1"
  = "(Command (Args1 ((Mnemonic \"INC\"), (Add ((Reg \"RAX\"), (Const \
     \"1\"))))))"

let%test _ =
  test_p code_line_parser show_code_section "inc rax, rax"
  = ": Invalid count of arguments"

let%test _ =
  test_p code_line_parser show_code_section "inc rax, rax, rax"
  = ": Invalid count of arguments"

let%test _ =
  test_p data_line_parser show_var "a dd 1"
  = "(Variable (\"a\", (DataType \"DD\"), [\"1\"]))"

let%test _ =
  test_p data_line_parser show_var "a dd 1, 2"
  = "(Variable (\"a\", (DataType \"DD\"), [\"1\"; \"2\"]))"

let%test _ =
  test_p data_line_parser show_var "a   dd    1   ,     2"
  = "(Variable (\"a\", (DataType \"DD\"), [\"1\"; \"2\"]))"

let%test _ =
  test_p data_line_parser show_var "a dD 1"
  = "(Variable (\"a\", (DataType \"DD\"), [\"1\"]))"

let%test _ =
  test_p data_line_parser show_var "A dd 1"
  = "(Variable (\"A\", (DataType \"DD\"), [\"1\"]))"

let%test _ =
  test_p data_line_parser show_var "dd dd 1"
  = ": Var's name must not be equal the name of reg or datatype"

let%test _ = test_p sec_parser show_ast "section .test" = ": Invalid section"
let%test _ = test_p sec_parser show_ast "section .code" = "(Code [])"
let%test _ = test_p sec_parser show_ast "section .text" = "(Code [])"
let%test _ = test_p sec_parser show_ast "section .data" = "(Data [])"

let%test _ =
  test_p sec_parser show_ast "section .code ret"
  = "(Code [(Command (Args0 (Mnemonic \"RET\")))])"

let%test _ =
  test_p sec_parser show_ast "section .code   ret   "
  = "(Code [(Command (Args0 (Mnemonic \"RET\")))])"

let%test _ =
  test_p sec_parser show_ast "section .code ret ret" = ": end_of_input"

let%test _ =
  match
    parse_string ~consume:All sec_parser "section .code inc rax inc rax"
  with
  | Ok v ->
      v
      = Code
          [
            Command (Args1 (Mnemonic "INC", Reg "RAX"));
            Command (Args1 (Mnemonic "INC", Reg "RAX"));
          ]
  | _ -> false

let%test _ =
  test_p sec_parser show_ast "section .data a dd 1"
  = "(Data [(Variable (\"a\", (DataType \"DD\"), [\"1\"]))])"

let%test _ =
  match parse_string ~consume:All sec_parser "section .data a dd 1 b dd 2" with
  | Ok v ->
      v
      = Data
          [
            Variable ("a", DataType "DD", [ "1" ]);
            Variable ("b", DataType "DD", [ "2" ]);
          ]
  | _ -> false

let%test _ =
  match
    parse_string ~consume:All parser "section .data a dd 1 section .text ret"
  with
  | Ok v ->
      v
      = [
          Data [ Variable ("a", DataType "DD", [ "1" ]) ];
          Code [ Command (Args0 (Mnemonic "RET")) ];
        ]
  | _ -> false

let%test _ =
  match
    parse_string ~consume:All parser
      "section .data a dd 1 section .data section .text ret"
  with
  | Ok v ->
      v
      = [
          Data [ Variable ("a", DataType "DD", [ "1" ]) ];
          Data [];
          Code [ Command (Args0 (Mnemonic "RET")) ];
        ]
  | _ -> false

let%test _ =
  match
    parse_string ~consume:All parser
      "section .data a dd 1 section .text ret section .text ret"
  with
  | Error msg -> msg = ": end_of_input"
  | _ -> false
