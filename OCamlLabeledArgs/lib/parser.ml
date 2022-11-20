(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
(* -------------------- Basic syntax -------------------- *)

(** A subset of OCaml keywords, that are used in our mini language *)
let keywords =
  [ "and"
  ; "do"
  ; "else"
  ; "false"
  ; "for"
  ; "fun"
  ; "function"
  ; "if"
  ; "in"
  ; "let"
  ; "mod"
  ; "nonrec"
  ; "of"
  ; "rec"
  ; "then"
  ; "to"
  ; "true"
  ; "val"
  ; "while"
  ]
;;

let is_keyword s = List.mem s keywords

let ignored =
  scan_state `Whitespace (fun state c ->
    match state with
    | `Whitespace ->
      (match c with
       | '\x20' | '\x09' | '\x0d' | '\x0a' | '\x0c' -> Some `Whitespace
       | _ -> None))
  >>| fun _ -> ()
;;

(* Plain combinators for convenient spaces removal *)
let ( *~> ) a b = a *> ignored *> b
let ( <~* ) a b = a <* ignored <* b
let lift2' f a b = lift2 f (a <* ignored) b
let lift3' f a b c = lift3 f (a <* ignored) (b <* ignored) c

(* Combinator used for parsing binary operations *)
let chainl1 e op =
  let rec go acc = lift2' (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

(* Plain combinators for parentheses parsing *)
let between a b p = a *> p <* b
let pstring s = between ignored ignored (string s)
let parens p = between (pstring "(") (pstring ")") p

(* Primitive syntax recognition *)
let is_identifier_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '\'' | '_' -> true
  | _ -> false
;;

let is_number_char = function
  | '0' .. '9' -> true
  | _ -> false
;;

let number_chars = take_while1 is_number_char

(* ----------------- Variable names ------------------ *)
let identifier =
  let is_valid_first_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | _ -> false
  in
  peek_char
  >>= function
  | Some c when is_valid_first_char c ->
    take_while is_identifier_char
    >>= fun s -> if is_keyword s then fail "Invalid variable name" else return s
  | _ -> fail "Invalid variable name" (* FIXME: error handling *)
;;

(* -------------------- Constant -------------------- *)

let boolean =
  string "true" *> return (Bool true) <|> string "false" *> return (Bool false)
;;

let integer =
  option "" (string "+" <|> string "-")
  >>= fun sign ->
  number_chars >>= fun whole -> return (Int (int_of_string (sign ^ whole)))
;;

let unit = parens ignored *> return Unit

(* ------------------- Operators -------------------- *)
let plus = ignored *> string "+" *> return (fun x y -> Binop (Plus, x, y))
let minus = ignored *> string "-" *> return (fun x y -> Binop (Minus, x, y))
let mult = ignored *> string "*" *> return (fun x y -> Binop (Mult, x, y))
let divide = ignored *> string "/" *> return (fun x y -> Binop (Divide, x, y))
let _mod = ignored *> string "mod" *> return (fun x y -> Binop (Mod, x, y))
let eq = ignored *> string "=" *> return (fun x y -> Binop (Eq, x, y))
let neq = ignored *> string "!=" *> return (fun x y -> Binop (Neq, x, y))
let lt = ignored *> string "<" *> return (fun x y -> Binop (Lt, x, y))
let ltq = ignored *> string "<=" *> return (fun x y -> Binop (Ltq, x, y))
let gt = ignored *> string ">" *> return (fun x y -> Binop (Gt, x, y))
let gtq = ignored *> string ">=" *> return (fun x y -> Binop (Gtq, x, y))

(* let op_presedence =
  [ eq <|> neq <|> lt <|> ltq <|> gt <|> gtq; plus <|> minus; mult <|> divide <|> _mod ]
;; *)

(* -------------------- Expressions -------------------- *)

(* let expr_parser_builder expr_type parsers =
  fix @@ fun _ -> lift expr_type (choice parsers)
;; *)

(* Helper functions for desugaring syntactic sugar *)
let rec desugar_lambda e = function
  | [] -> e
  | v :: vs -> Fun (v, desugar_lambda e vs)
;;

let desugar_let e e' names include_rec =
  let desugar_to_tuple e e' = function
    | [] as vs -> failwith (String.concat "::" vs) (* FIXME: error handling *)
    | [ v ] -> v, e, e'
    | v :: vs -> v, desugar_lambda e vs, e'
  in
  let v, ne, ne' = desugar_to_tuple e e' names in
  if include_rec then LetRec (v, ne, ne') else Let (v, ne, ne')
;;

(* Dispatch table for mutually recursive expression parsers *)
type expr_dispatch = { expr : expr_dispatch -> expr t }

(* Expression parsers *)
let expr_d =
  let var_parser =
    let var_expr x = Var x in
    fix @@ fun _ -> lift var_expr (choice [ identifier ])
  in
  let const_parser =
    let const_expr x = Const x in
    fix @@ fun _ -> lift const_expr (choice [ boolean; integer; unit ])
  in
  let binop_parser =
    let num = integer >>| fun n -> Const n in
    let var = identifier >>| fun v -> Var v in
    fix
    @@ fun binop_expr ->
    let factor = parens binop_expr <|> num <|> var in
    let term_mult = chainl1 factor (mult <|> divide <|> _mod) in
    let term_add = chainl1 term_mult (plus <|> minus) in
    chainl1 term_add (eq <|> neq <|> lt <|> ltq <|> gt <|> gtq)
  in
  let fun_parser d =
    fix
    @@ fun self ->
    choice
      [ parens self
      ; (string "fun" *~> many_till identifier (ignored *> string "->")
        >>= fun fun_args ->
        ignored *> d.expr d >>= fun e -> return (desugar_lambda e fun_args))
      ]
  in
  (*                     *)
  (* TODO: implement App *)
  (*                     *)
  let if_then_else_parser d =
    let if_expr cond tbody fbody = IfThenElse (cond, tbody, fbody) in
    fix
    @@ fun self ->
    choice
      [ parens self
      ; (let cond = string "if" *~> d.expr d in
         let tbody = string "then" *~> d.expr d in
         let fbody = option (Const Unit) (string "else" *~> d.expr d) in
         lift3' if_expr cond tbody fbody)
      ]
  in
  (* and is not supported *)
  let let_parser d =
    fix
    @@ fun self ->
    choice
      [ parens self
      ; (string "let" *~> option "" (string "rec")
        >>= fun _rec ->
        ignored *> many1 identifier
        >>= fun var_list ->
        ignored *> char '=' *~> d.expr d
        >>= fun e ->
        ignored *> string "in" *~> d.expr d
        >>= fun e' ->
        match _rec with
        | "" -> return (desugar_let e e' var_list false)
        | "rec" -> return (desugar_let e e' var_list true)
        | _ -> fail "Error in parsing let" (* FIXME: error handling *))
      ]
  in
  let expr d =
    choice
      [ binop_parser
      ; const_parser
      ; fun_parser d
      ; if_then_else_parser d
      ; let_parser d
      ; var_parser
      ]
  in
  { expr }
;;

let expr_parser = expr_d.expr expr_d

(* ------------------------------------------------------ *)
(* ------------------ Top-level parser ------------------ *)
(* ------------------------------------------------------ *)

let parse p s = parse_string ~consume:All p s