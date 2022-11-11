open Angstrom
open Ast

let parse p s = parse_string ~consume:All p s

let is_keyword = function
  | "\\%token" | "\\%main" | "\\%%start" -> true
  | _ -> false
;;

let is_whitespace = function
  | ' ' | '\t' -> true
  | _ -> false
;;

let is_end_of_line = function
  | '\n' | '\r' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_valid_id_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let space = take_while is_whitespace
let space1 = take_while1 is_whitespace
let token s = space *> string s

let number =
  let integer = take_while1 is_digit in
  space *> integer >>= fun whole -> return @@ CINT (int_of_string whole);;
