open Ast

type input = char list

type 'a parse_result =
  | Failed
  | Parsed of 'a * input

type 'a parser = input -> 'a parse_result

let return x s = Parsed (x, s)

let satisfy cond = function
  | h :: tl when cond h -> return h tl
  | _ -> Failed
;;

let char c = satisfy (Char.equal c)

let digit_c =
  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false
  in
  satisfy is_digit
;;

let%test _ = char 'a' [ 'a'; 'b' ] = return 'a' [ 'b' ]
let%test _ = char 'b' [ 'a'; 'b' ] = Failed
let%test _ = digit_c [ '1'; '2' ] = return '1' [ '2' ]
let%test _ = digit_c [ 'a'; '1' ] = Failed

let ( >>= ) p f s =
  match p s with
  | Failed -> Failed
  | Parsed (h, tl) -> f h tl
;;

let ( *> ) p1 p2 = p1 >>= fun _ -> p2
let ( <* ) p1 p2 = p1 >>= fun h -> p2 >>= fun _ -> return h

let%test _ =
  let p1 = char 'a' *> char 'b' in
  p1 [ 'a'; 'b' ] = return 'b' []
;;

let%test _ =
  let p1 = char 'b' <* char 'a' in
  p1 [ 'b'; 'a' ] = return 'b' []
;;

let digit_n = digit_c >>= fun c -> return (Char.code c - Char.code '0')

let%test _ = digit_n [ '1'; '2' ] = return 1 [ '2' ]
let%test _ = digit_n [ 'a'; '1' ] = Failed

let many p =
  let rec helper s =
    match p s with
    | Failed -> return [] s
    | Parsed (x, tl) -> (helper >>= fun xs -> return (x :: xs)) tl
  in
  helper
;;

let many1 p = p >>= fun x -> many p >>= fun xs -> return (x :: xs)

let%test _ = many (char 'a') [ 'a'; 'a'; 'b' ] = return [ 'a'; 'a' ] [ 'b' ]
let%test _ = many (char 'a') [ 'b'; 'a'; 'b' ] = return [] [ 'b'; 'a'; 'b' ]
let%test _ = many1 (char 'a') [ 'a'; 'a'; 'b' ] = return [ 'a'; 'a' ] [ 'b' ]
let%test _ = many1 (char 'a') [ 'b'; 'a'; 'b' ] = Failed

let ( <|> ) p1 p2 s =
  match p1 s with
  | Failed -> p2 s
  | ok -> ok
;;

let conde = function
  | [] -> raise (Invalid_argument "No conds in conde")
  | h :: tl -> List.fold_left ( <|> ) h tl
;;

let%test _ =
  let p = char 'a' <|> char 'b' in
  p [ 'a'; 'b' ] = return 'a' [ 'b' ]
;;

let%test _ =
  let p = char 'a' <|> char 'b' in
  p [ 'b'; 'a' ] = return 'b' [ 'a' ]
;;

let%test _ =
  let p = char 'a' <|> char 'b' in
  many p [ 'a'; 'b' ] = return [ 'a'; 'b' ] []
;;

let%test _ =
  let p = conde [ char 'a'; char 'b'; digit_c ] in
  many p [ 'a'; 'b'; '1'; '2' ] = return [ 'a'; 'b'; '1'; '2' ] []
;;

let digit = digit_c >>= fun c -> return (Char.code c - Char.code '0')

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_whitespace = function
  | ' ' | '\n' | '\r' | '\t' -> true
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

let is_128bitreg = function
  | "XMM0" | "XMM1" | "XMM2" | "XMM3" | "XMM4" | "XMM5" | "XMM6" | "XMM7" -> true
  | _ -> false
;;

let is_register s =
  List.fold_left
    ( || )
    false
    [ is_8bitreg s; is_16bitreg s; is_32bitreg s; is_64bitreg s; is_128bitreg s ]
;;

let is_0argsmn = function
  | "RET" | "SYSCALL" -> true
  | _ -> false
;;

let is_1argsmn = function
  | "PUSH"
  | "POP"
  | "INC"
  | "DEC"
  | "NOT"
  | "NEG"
  | "JMP"
  | "JE"
  | "JNE"
  | "JG"
  | "JGE"
  | "JL"
  | "JLE"
  | "CALL" -> true
  | _ -> false
;;

let is_2argsmn = function
  | "MOV"
  | "ADD"
  | "SUB"
  | "IMUL"
  | "AND"
  | "OR"
  | "XOR"
  | "SHL"
  | "SHR"
  | "CMP"
  | "ADDPD"
  | "SUBPD"
  | "MULPD"
  | "MOVAPD" -> true
  | _ -> false
;;

let is_mnemonic s =
  List.fold_left ( || ) false [ is_0argsmn s; is_1argsmn s; is_2argsmn s ]
;;

let is_section = function
  | "CODE" | "TEXT" | "DATA" | "CONST" -> true
  | _ -> false
;;

let letter = satisfy is_letter
let whitespace = satisfy is_whitespace
let spaces = many1 whitespace
let trim x = spaces *> x <* spaces
let word = many1 letter
let integer = many1 digit_c

let code =
  "\n\
  \            mov rax, 1\n\
  \            mov rdi, 1\n\
  \            mov rsi, message\n\
  \            mov rdx, 14\n\
  \            syscall \n"
;;

let expr_parser =
  let parents p = trim (char '(' *> trim p <* char ')') in
  let add = char '+' *> return (fun x y -> Add (x, y)) in
  let sub = char '-' *> return (fun x y -> Sub (x, y)) in
  let mul = char '*' *> return (fun x y -> Mul (x, y)) in
  let div = char '%' *> return (fun x y -> Div (x, y)) in
  let num =
    many digit_n
    >>= fun list -> return (Const (List.fold_right (fun x y -> (y * 10) + x) list 0))
  in
  let label_of_reg =
    word
    >>= fun x ->
    let w = String.uppercase_ascii (Base.String.of_char_list x) in
    if is_register w then return (Reg w) else return (Label w)
  in
  let arg = num <|> label_of_reg in
  let rec p s =
    conde
      [ parents p
      ; (arg
        >>= fun h -> conde [ add; sub; mul; div ] >>= fun op tl -> return (op h (p tl))
      ]
      s
  in
  p
;;
 (*WHAT IS CHAINL1 AND HOW IT SOLVES MY PROBLEM? I WANT KNOW*)