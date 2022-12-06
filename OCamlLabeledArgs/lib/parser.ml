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

let is_ignored = function
  | '\x20' | '\x09' | '\x0d' | '\x0a' | '\x0c' -> true
  | _ -> false
;;

let ignored = skip_while is_ignored
let required_ws = take_while1 is_ignored

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

(* ----------------- Function arguments ---------------- *)

(* Helper type for representing the function arguments *)
type fun_argument =
  { label : arg_label
  ; name : id
  ; default_value : expr option
  }

let label_parser =
  let label =
    string "~"
    <|> string "?"
    >>= fun parsed_arg_type ->
    identifier
    <* option ":" (string ":")
    >>= fun name ->
    match parsed_arg_type with
    | "~" -> return (ArgLabeled name)
    | "?" -> return (ArgOptional name)
    | _ -> fail "Error parsing the label of the argument"
  in
  ignored *> label
;;

(* -------------------- Expressions -------------------- *)

(* Helper functions for desugaring syntactic sugar *)

(* For transforming "Fun x y -> e" into Fun (x, Fun (y, e)) *)
let rec desugar_lambda exp = function
  | [] -> exp
  | a :: args -> Fun (a.label, a.default_value, a.name, desugar_lambda exp args)
;;

let desugar_let exp in_exp names include_rec =
  let desugar_to_tuple exp in_exp = function
    | [] -> failwith "Error desugaring let" (* FIXME: error handling *)
    | [ a ] -> a, exp, in_exp
    | a :: args -> a, desugar_lambda exp args, in_exp
  in
  let a, new_exp, new_in_exp = desugar_to_tuple exp in_exp names in
  if include_rec
  then LetRec (a.name, new_exp, new_in_exp)
  else Let (a.name, new_exp, new_in_exp)
;;

let desugar_def args exp include_rec =
  let a, exp =
    match args with
    | [] -> failwith "Error desugaring letdef" (* FIXME: error handling *)
    | [ a ] -> a, exp
    | _a :: _args -> _a, desugar_lambda exp _args
  in
  if include_rec then a.name, LetRec (a.name, exp, Var a.name) else a.name, exp
;;

(* Helper type for representing application arguments, that might be labeled *)
type app_argument =
  { label : arg_label
  ; expr : expr
  }

let rec desugar_app exp = function
  | [] -> exp
  | a :: args -> App (desugar_app exp args, a.label, a.expr)
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
  let fun_argument_parser d =
    ignored *> option ArgNoLabel label_parser
    >>= fun label ->
    (* If there is no label then parse the argument name *)
    (* If there is a label then parse the argument name or just the label:
        let f ~name1:name1 ~name2:name2 = name1 + name2
        let f ~name1 ~name2 = name1 + name2
       are equivalent *)
    (* If there is an optional label then parse the argument label, 
       name and the (optional) default value in parentheses:
        let f ?arg:(arg = expr1) -> expr2
       where expr1 is the default value or just 
        let f ?arg -> expr2 *)
    match label with
    | ArgNoLabel ->
      identifier
      <|> parens identifier
      <|> parens (string "" <|> required_ws)
      >>= fun name -> return { label; name; default_value = None }
    | ArgLabeled _ ->
      option "" identifier
      <|> parens identifier
      >>= fun name -> return { label; name; default_value = None }
    | ArgOptional _ ->
      required_ws
      >>= (fun _ -> return { label; name = ""; default_value = None })
      <|> parens
            (identifier
            >>= fun name ->
            ignored *> string "=" *~> d.expr d
            >>= fun e -> return { label; name; default_value = Some e })
      <?> "fun_argument_parser"
  in
  let fun_parser d =
    fix
    @@ fun self ->
    choice
      [ parens self
      ; (string "fun" (* TODO: fix keywords parsing (i.e. "funi" parses successfully) *)
         *~> many_till (fun_argument_parser d) (pstring "->")
        >>= fun fun_args -> d.expr d >>= fun e -> return (desugar_lambda e fun_args))
      ]
    <?> "fun_parser"
  in
  let app_parser d =
    let app_argument_parser =
      ignored *> option ArgNoLabel label_parser
      >>= fun label ->
      ignored *> d.expr d
      >>= fun expr ->
      match label with
      | ArgNoLabel | ArgLabeled _ -> return { label; expr }
      | ArgOptional _ -> fail "You can not make a funcall with optional argument syntax"
    in
    fix
    @@ fun self ->
    choice
      [ parens self
      ; (ignored *> var_parser
        >>= fun n -> many1 app_argument_parser >>= fun args -> return (desugar_app n args)
        )
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
        ignored *> many1 (fun_argument_parser d)
        >>= fun var_list ->
        ignored *> string "=" *~> d.expr d
        >>= fun exp ->
        ignored *> string "in" *~> d.expr d
        >>= fun in_exp ->
        match _rec with
        | "" -> return (desugar_let exp in_exp var_list false)
        | "rec" -> return (desugar_let exp in_exp var_list true)
        | _ -> fail "Error in parsing let")
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
        ignored *> many1 (fun_argument_parser d)
        >>= fun var_list ->
        ignored *> string "=" *~> d.expr d
        >>= fun exp ->
        match _rec with
        | "" -> return (desugar_def var_list exp false)
        | "rec" -> return (desugar_def var_list exp true)
        | _ -> fail "Error in parsing letdef")
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