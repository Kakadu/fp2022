open Angstrom
open Ast

let is_ws = function
  | ' ' | '\n' | '\t' | '\r' -> true
  | _ -> false
;;

let whitespaces = skip_while is_ws
let trim x = whitespaces *> x <* whitespaces
let parens p = trim (char '(' *> p <* char ')')

let conde = function
  | [] -> raise (Invalid_argument "No conds in conde")
  | x :: xs -> List.fold_left ( <|> ) x xs
;;

let is_num = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_ch = function
  | 'A' .. 'Z' | 'a' .. 'z' -> true
  | _ -> false
;;

let is_8bitreg = function
  | "AH" | "AL" | "BH" | "BL" | "CH" | "CL" | "DH" | "DL" -> true
  | _ -> false
;;

let is_16bitreg = function
  | "AX" | "BX" | "CX" | "DX" -> true
  | _ -> false
;;

let is_32bitreg = function
  | "EAX" | "EBX" | "ECX" | "EDX" | "ESI" | "EDI" | "ESP" | "EBP" -> true
  | _ -> false
;;

let is_64bitreg = function
  | "RAX" | "RBX" | "RCX" | "RDX" | "RSP" | "RBP" | "RSI" | "RDI" -> true
  | _ -> false
;;

let is_hex_digit = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_reg s =
  List.fold_left
    ( || )
    false
    [ is_8bitreg s; is_16bitreg s; is_32bitreg s; is_64bitreg s ]
;;

let is_arg0 = function
  | "RET" | "SYSCALL" -> true
  | _ -> false
;;

let is_arg1 = function
  | "PUSH" | "POP" | "INC" | "DEC" | "IDIV" | "NOT" | "NEG" | "CALL" -> true
  | _ -> false
;;

let is_arg2 = function
  | "MOV" | "LEA" | "ADD" | "SUB" | "IMUL" | "AND" | "XOR" | "OR" | "SHL" | "SHR" | "CMP"
    -> true
  | _ -> false
;;

let is_jmp = function
  | "JMP" | "JE" | "JNE" | "JZ" | "JG" | "JGE" | "JL" | "JLE" -> true
  | _ -> false
;;

let is_data_dec = function
  | "DB" | "DW" | "DD" | "DQ" -> true
  | _ -> false
;;

let is_mnemonic s = is_arg0 s || is_arg1 s || is_arg2 s || is_jmp s
let digit_c = satisfy is_num
let digit = digit_c >>= fun c -> return (Char.code c - Char.code '0')
let nums = take_while1 is_num
let word = take_while1 is_ch

let expr_parser =
  let add = char '+' *> return (fun x y -> Add (x, y)) in
  let sub = char '-' *> return (fun x y -> Sub (x, y)) in
  let mul = char '*' *> return (fun x y -> Mul (x, y)) in
  let div = char '/' *> return (fun x y -> Div (x, y)) in
  let sign = option "" (string "+" <|> string "-") in
  let hex_prefix = option "" (string "0x") in
  let num =
    sign
    >>= fun s ->
    hex_prefix
    >>= fun p ->
    nums >>= fun n -> return (Const (int_of_string @@ String.concat "" [ s; p; n ]))
  in
  let reg =
    word
    >>= fun x ->
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
;;

let code_line_parser =
  let label = trim @@ word <* char ':' >>= fun x -> return (Id (Label x)) in
  let mnem =
    trim @@ word
    >>= fun x ->
    let w = String.uppercase_ascii x in
    if is_mnemonic w then return (Mnemonic w) else failwith @@ "Invalid command " ^ x
  in
  let sep = trim @@ char ',' in
  let command (Mnemonic cmd) = cmd in
  let inst =
    mnem
    >>= fun cmd ->
    if is_jmp @@ command cmd
    then trim word >>= fun l -> return (Command (Jmp (cmd, Label l)))
    else
      trim @@ sep_by sep expr_parser
      >>= fun exprs ->
      match List.length exprs with
      | 0 -> return (Command (Args0 cmd))
      | 1 -> return (Command (Args1 (cmd, List.hd exprs)))
      | 2 -> return (Command (Args2 (cmd, List.hd exprs, List.nth exprs 1)))
      | _ -> failwith "Invalid count of arguments"
  in
  label <|> inst
;;

let data_line_parser =
  let data_t =
    trim word
    >>= fun x ->
    let w = String.uppercase_ascii x in
    if is_data_dec w then return (DataType w) else failwith "Invalud datatype"
  in
  let var =
    trim word
    >>= fun x ->
    let w = String.uppercase_ascii x in
    if is_reg w || is_data_dec w
    then failwith "Var's name must not be equal the name of reg or datatype"
    else return (fun dt y -> Variable (x, dt, y))
  in
  let sep = trim @@ char ',' in
  var
  >>= fun v ->
  data_t >>= fun dt -> trim @@ sep_by sep (word <|> nums) >>= fun l -> return (v dt l)
;;

let sec_parser =
  trim @@ (string "section" *> whitespaces *> char '.' *> word)
  >>= function
  | "code" | "text" -> many code_line_parser >>= fun values -> return (Code values)
  | "data" -> many data_line_parser >>= fun values -> return (Data values)
  | _ -> failwith "Invalid section"
;;

let parser = many sec_parser

let eval str =
  match parse_string ~consume:All parser str with
  | Ok v -> v
  | Error msg -> failwith msg
;;
