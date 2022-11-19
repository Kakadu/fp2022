open Angstrom
open Ast

let is_whitespace = function
  | ' ' | '\t' -> true
  | _ -> false
;;

let is_end_of_line = function
  | '\n' | '\r' -> true
  | _ -> false
;;

let is_token_keyword = function
  | "%token" -> true
  | _ -> false
;;

let is_token_char = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_start_keyword = function
  | "%start" -> true
  | _ -> false
;;

let is_nonterm_char = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_not_colon = function
  | '|' -> false
  | _ -> true
;;

let space = take_while is_whitespace
let space1 = take_while1 is_whitespace
let token t = space *> string "%token" *> space *> t

let start =
  space *> string "start"
  >>= function
  | s when is_start_keyword s -> take_while1 is_nonterm_char
  | _ -> fail "Invalid identifier"
;;

let procent_procent = char '%' *> char '%'

let nonterm =
  space *> take_while1 is_nonterm_char <* char ':' (* main: *) >>= fun x -> return x
;;

let nontem_comp = space *> char '|' *> take_while1 is_not_colon

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let expr : string t =
  fix (fun e -> space *> string "%token" *> space *> e >>= fun x -> return x)
;;

let eval (str : string) : string =
  match parse_string ~consume:All expr str with
  | Ok v -> v
  | Error msg -> failwith msg
;;
