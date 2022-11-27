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

(* Helper functions for desugaring syntactic sugar *)
let rec desugar_lambda exp = function
  | [] -> exp
  | v :: vs -> Fun (v, desugar_lambda exp vs)
;;

let desugar_let exp in_exp names include_rec =
  let desugar_to_tuple exp in_exp = function
    | [] as vs -> failwith (String.concat "::" vs) (* FIXME: error handling *)
    | [ v ] -> v, exp, in_exp
    | v :: vs -> v, desugar_lambda exp vs, in_exp
  in
  let v, new_exp, new_in_exp = desugar_to_tuple exp in_exp names in
  if include_rec then LetRec (v, new_exp, new_in_exp) else Let (v, new_exp, new_in_exp)
;;

let desugar_def vs exp include_rec =
  let ((v, exp) as t) =
    match vs with
    | [] -> failwith (String.concat "::" vs) (* FIXME: error handling *)
    | [ v ] -> v, exp
    | x :: xs -> x, desugar_lambda exp xs
  in
  if include_rec then v, LetRec (v, exp, Var v) else t
;;

let arg_parser =
  let arg_no_label name = ArgNoLabel name in
  let arg_labelled name = ArgLabelled name in
  let arg_optional name = ArgOptional name in
  let arg =
    option "" (string "~" <|> string "?")
    >>= fun parsed_arg_type ->
    match parsed_arg_type with
    | "~" -> lift arg_labelled identifier
    | "?" -> lift arg_optional identifier
    | _ -> lift arg_no_label identifier
  in
  fix @@ fun self -> choice [ parens self; ignored *> arg ]
;;

(* Dispatch table for mutually recursive parsers *)
type type_dispatch =
  { expr : type_dispatch -> expr t
  ; definition : type_dispatch -> definition t
  }

(* Main parsers *)
let type_d =
  let var_parser =
    let var_expr x = Var x in
    lift var_expr identifier <?> "var_parser"
  in
  let const_parser =
    let const_expr x = Const x in
    lift const_expr (choice [ boolean; integer; unit ]) <?> "const_parser"
  in
  let fun_parser d =
    fix
    @@ fun self ->
    choice
      [ parens self
      ; (string "fun" (* TODO: fix keywords parsing (i.e. "funi" parses successfully) *)
         *~> many_till (ignored *> identifier) (pstring "->")
        >>= fun fun_args -> d.expr d >>= fun e -> return (desugar_lambda e fun_args))
      ]
    <?> "fun_parser"
  in
  (* Application of functions with multiple arguments is sketchy *)
  let app_parser d =
    let app_expr name arg = App (name, arg) in
    fix
    @@ fun self ->
    choice
      [ parens self
      ; (let name = var_parser in
         let arg = d.expr d in
         lift2' app_expr name (ignored *> arg))
      ]
    <?> "app_parser"
  in
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
    <?> "if_the_else_parser"
  in
  (* and is not supported *)
  let let_parser d =
    fix
    @@ fun self ->
    choice
      [ parens self
      ; (string "let" *~> option "" (string "rec")
        >>= fun _rec ->
        ignored *> many1 (ignored *> identifier)
        >>= fun var_list ->
        ignored *> char '=' *~> d.expr d
        >>= fun exp ->
        ignored *> string "in" *~> d.expr d
        >>= fun in_exp ->
        match _rec with
        | "" -> return (desugar_let exp in_exp var_list false)
        | "rec" -> return (desugar_let exp in_exp var_list true)
        | _ -> fail "Error in parsing let" (* FIXME: error handling *))
      ]
    <?> "let_parser"
  in
  let def_parser d =
    fix
    @@ fun self ->
    choice
      [ parens self
      ; (string "let" *~> option "" (string "rec")
        >>= fun _rec ->
        ignored *> many1 (ignored *> identifier)
        >>= fun var_list ->
        ignored *> char '=' *~> d.expr d
        >>= fun exp ->
        match _rec with
        | "" -> return (desugar_def var_list exp false)
        | "rec" -> return (desugar_def var_list exp true)
        | _ -> fail "Error in parsing letdef" (* FIXME: error handling *))
      ]
    <?> "def_parser"
  in
  let binop_parser d =
    fix
    @@ fun binop_expr ->
    let factor d =
      parens binop_expr <|> app_parser d <|> let_parser d <|> const_parser <|> var_parser
    in
    let term_mult = chainl1 (factor d) (mult <|> divide <|> _mod) in
    let term_add = chainl1 term_mult (plus <|> minus) in
    chainl1 term_add (eq <|> neq <|> ltq <|> lt <|> gtq <|> gt) <?> "binop_parser"
  in
  let expr d =
    app_parser d
    <|> binop_parser d
    <|> let_parser d
    <|> fun_parser d
    <|> if_then_else_parser d
    <|> const_parser
    <|> var_parser
  in
  let definition d = def_parser d in
  { expr; definition }
;;

let expr_parser = type_d.expr type_d
let definition_parser = type_d.definition type_d

(* ------------------------------------------------------ *)
(* ------------------ Top-level parser ------------------ *)
(* ------------------------------------------------------ *)

let parse p s = parse_string ~consume:All p s