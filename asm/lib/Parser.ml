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
  | "PUSH" | "POP" | "INC" | "DEC" | "NOT" | "NEG" | "CALL" | "JMP" | "JE"
  | "JNE" | "JZ" | "JG" | "JGE" | "JL" | "JLE" ->
      true
  | _ -> false

let is_arg2 = function
  | "MOV" | "ADD" | "SUB" | "IMUL" | "AND" | "XOR" | "OR" | "SHL" | "SHR"
  | "CMP" ->
      true
  | _ -> false

let is_data_dec = function "DB" | "DW" | "DD" | "DQ" -> true | _ -> false
let is_mnemonic s = is_arg0 s || is_arg1 s || is_arg2 s

(** parse integer like 42727 *)
let nums = take_while1 is_num

let hex_nums = take_while1 is_hex_digit

(** parse word of letters *)
let word = take_while1 is_ch

(** parse expression that may contain +, -, *, /, registers and int constants in hex and decimal forms then return Ast.expr where operations have prio *)
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
    match String.uppercase_ascii x with
    | w when is_reg w -> return (Reg w)
    | _ -> return (Lab (Label x))
  in
  let var = char '%' *> word >>= fun x -> return (Var x) in
  let arg = reg <|> num <|> var in
  let chainl1 e op =
    let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
    e >>= fun init -> go init
  in
  fix (fun expr ->
      let factor = parens expr <|> trim arg in
      let term = trim @@ chainl1 factor (mul <|> div) in
      trim @@ chainl1 term (add <|> sub))

(** parse one line of code: mnemonic and her argumets or label then return Ast.code_section *)
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
    trim @@ sep_by sep expr_parser >>= function
    | [] when is_arg0 @@ command cmd -> return (Command (Args0 cmd))
    | [ arg ] when is_arg1 @@ command cmd -> return (Command (Args1 (cmd, arg)))
    | [ arg1; arg2 ] when is_arg2 @@ command cmd ->
        return (Command (Args2 (cmd, arg1, arg2)))
    | _ -> fail "Invalid count of arguments"
  in
  label <|> inst

(** parse one line of data section: type of const and const then return Ast.var t *)
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
  var <* char ':' >>= fun v ->
  data_t >>= fun dt ->
  trim @@ sep_by sep (word <|> nums) >>= fun l -> return (v dt l)

(** parse one of two possible sections and then parse section then return Ast.ast *)
let sec_parser =
  trim @@ (string "section" *> whitespaces *> char '.' *> word) >>= function
  | "code" | "text" ->
      many code_line_parser >>= fun values -> return (Code values)
  | "data" -> many data_line_parser >>= fun values -> return (Data values)
  | _ -> fail "Invalid section"

(** main paresr *)
let parser = many sec_parser

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

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr_parser "0");
  [%expect {|(Const "0")|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr_parser "0xa");
  [%expect {|(Const "0xa")|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr_parser "    0xa   ");
  [%expect {|(Const "0xa")|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr_parser "rax");
  [%expect {|(Reg "RAX")|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr_parser "rAx");
  [%expect {|(Reg "RAX")|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr_parser "%var");
  [%expect {|(Var "var")|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr_parser "%vAr");
  [%expect {|(Var "vAr")|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr_parser "labl");
  [%expect {|(Lab (Label "labl"))|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr_parser "lAbl");
  [%expect {|(Lab (Label "lAbl"))|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr_parser "0 + 0x0");
  [%expect {|(Add ((Const "0"), (Const "0x0")))|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr_parser "0 + rax");
  [%expect {|(Add ((Const "0"), (Reg "RAX")))|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr_parser "0 + %var");
  [%expect {|(Add ((Const "0"), (Var "var")))|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr_parser "0+0x0");
  [%expect {|(Add ((Const "0"), (Const "0x0")))|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr_parser "0      +     0x0");
  [%expect {|(Add ((Const "0"), (Const "0x0")))|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr_parser "0 + (0x0)");
  [%expect {|(Add ((Const "0"), (Const "0x0")))|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr_parser "0 + (    0x0   )   ");
  [%expect {|(Add ((Const "0"), (Const "0x0")))|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr_parser "(0 + (0x0))");
  [%expect {|(Add ((Const "0"), (Const "0x0")))|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr_parser "0 + 1 * 2");
  [%expect {|(Add ((Const "0"), (Mul ((Const "1"), (Const "2")))))|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr_parser "0 * 1 + 2");
  [%expect {|(Add ((Mul ((Const "0"), (Const "1"))), (Const "2")))|}]

let%expect_test _ =
  print_string @@ show_expr (pr_opt expr_parser "0 * (1 + 2)");
  [%expect {|(Mul ((Const "0"), (Add ((Const "1"), (Const "2")))))|}]

let%expect_test _ =
  print_string @@ show_code_section (pr_opt code_line_parser "ret");
  [%expect {|(Command (Args0 (Mnemonic "RET")))|}]

let%expect_test _ =
  print_string @@ show_code_section (pr_opt code_line_parser "rEt");
  [%expect {|(Command (Args0 (Mnemonic "RET")))|}]

let%expect_test _ =
  print_string
  @@ show_code_section (pr_opt code_line_parser "       \nret    \n\n");
  [%expect {|(Command (Args0 (Mnemonic "RET")))|}]

let%expect_test _ =
  print_string @@ show_code_section (pr_opt code_line_parser "inc rax");
  [%expect {|(Command (Args1 ((Mnemonic "INC"), (Reg "RAX"))))|}]

let%expect_test _ =
  print_string
  @@ show_code_section
       (pr_opt code_line_parser "    inc                  rax           ");
  [%expect {|(Command (Args1 ((Mnemonic "INC"), (Reg "RAX"))))|}]

let%expect_test _ =
  print_string @@ show_code_section (pr_opt code_line_parser "inc rax + 1");
  [%expect
    {|(Command (Args1 ((Mnemonic "INC"), (Add ((Reg "RAX"), (Const "1"))))))|}]

let%expect_test _ =
  print_string @@ show_var (pr_opt data_line_parser "a: dd 1");
  [%expect {|(Variable ("a", (DataType "DD"), ["1"]))|}]

let%expect_test _ =
  print_string @@ show_var (pr_opt data_line_parser "a: dd 1, 2");
  [%expect {|(Variable ("a", (DataType "DD"), ["1"; "2"]))|}]

let%expect_test _ =
  print_string @@ show_var (pr_opt data_line_parser "a:   dd    1   ,     2");
  [%expect {|(Variable ("a", (DataType "DD"), ["1"; "2"]))|}]

let%expect_test _ =
  print_string @@ show_var (pr_opt data_line_parser "a: dD 1");
  [%expect {|(Variable ("a", (DataType "DD"), ["1"]))|}]

let%expect_test _ =
  print_string @@ show_var (pr_opt data_line_parser "A: dd 1");
  [%expect {|(Variable ("A", (DataType "DD"), ["1"]))|}]

let%expect_test _ =
  print_string @@ show_dir (pr_opt sec_parser "section .code");
  [%expect {|(Code [])|}]

let%expect_test _ =
  print_string @@ show_dir (pr_opt sec_parser "section .text");
  [%expect {|(Code [])|}]

let%expect_test _ =
  print_string @@ show_dir (pr_opt sec_parser "section .data");
  [%expect {|(Data [])|}]

let%expect_test _ =
  print_string @@ show_dir (pr_opt sec_parser "section .code ret");
  [%expect {|(Code [(Command (Args0 (Mnemonic "RET")))])|}]

let%expect_test _ =
  print_string @@ show_dir (pr_opt sec_parser "section .code   ret   ");
  [%expect {|(Code [(Command (Args0 (Mnemonic "RET")))])|}]

let%expect_test _ =
  print_string @@ show_dir (pr_opt sec_parser "section .code inc rax inc rax");
  [%expect
    {|
      (Code
         [(Command (Args1 ((Mnemonic "INC"), (Reg "RAX"))));
           (Command (Args1 ((Mnemonic "INC"), (Reg "RAX"))))])|}]

let%expect_test _ =
  print_string @@ show_dir (pr_opt sec_parser "section .data a: dd 1");
  [%expect {|(Data [(Variable ("a", (DataType "DD"), ["1"]))])|}]

let%expect_test _ =
  print_string @@ show_dir (pr_opt sec_parser "section .data a: dd 1 b: dd 2");
  [%expect
    {|
    (Data
       [(Variable ("a", (DataType "DD"), ["1"]));
         (Variable ("b", (DataType "DD"), ["2"]))])|}]

let%expect_test _ =
  print_string
  @@ show_ast (pr_opt parser "section .data a: dd 1 section .text ret");
  [%expect{|
    [(Data [(Variable ("a", (DataType "DD"), ["1"]))]);
      (Code [(Command (Args0 (Mnemonic "RET")))])] |}]

let%expect_test _ =
  print_string
  @@ show_ast
       (pr_opt parser "section .data a: dd 1 section .data section .text ret");
  [%expect
    {|
    [(Data [(Variable ("a", (DataType "DD"), ["1"]))]); (Data []);
      (Code [(Command (Args0 (Mnemonic "RET")))])]|}]
