(** Copyright 2021-2022, Ilya Shchuckin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

let pp_error ppf = function
  | `ParsingError s -> Format.fprintf ppf "%s" s
;;

let chainl1 e op =
  let rec go acc =
    lift2 (fun f x -> Compound { atom = f; terms = [ acc; x ] }) op e
    >>= go
    <|> return acc
  in
  e >>= fun init -> go init
;;

type error = [ `ParsingError of string ]

let is_layout_char = function
  | ' ' | '\t' | '\n' -> true
  | _ -> false
;;

let lay_seq = many (satisfy is_layout_char) (*layout sequence*)
let token s = lay_seq *> string s
let comma = lay_seq *> token ","

let infix_operator (prior : int) =
  let ops ops_list = lift (fun op -> Operator op) ops_list in
  match prior with
  | 1200 -> ops (token ":-")
  | 1000 -> ops comma
  | 700 -> ops (token "==" <|> token "=")
  | p -> fail ("No such operator with priority: " ^ string_of_int p)
;;

let prefix_operator = lift (fun op -> Operator op) (token "\\+")
let dot = lay_seq *> char '.' *> lay_seq
let bar = lay_seq *> char '|' *> lay_seq
let parens x = char '(' *> x <* char ')'
let rnd_brackets x = lay_seq *> char '(' *> x <* lay_seq <* char ')'
let brackets x = lay_seq *> char '[' *> x <* lay_seq <* char ']'
let name_c x = Name x
let num_c x = Num x
let atom_c x = Atom x
let var_c x = Var x

let is_alphanumeric = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let is_graphic_char = function
  | '#'
  | '$'
  | '&'
  | '*'
  | '+'
  | '-'
  | '.'
  | '/'
  | ':'
  | '<'
  | '='
  | '>'
  | '?'
  | '@'
  | '^'
  | '~' -> true
  | _ -> false
;;

let name =
  lift2
    (fun x y -> Name (String.make 1 x ^ y))
    (lay_seq
    *> (satisfy (function
          | 'a' .. 'z' -> true
          | '!' -> true
          | _ -> false)
       <|> satisfy is_graphic_char))
    (take_while is_alphanumeric)
;;

let string =
  let is_non_quote x =
    is_alphanumeric x
    || is_graphic_char x
    ||
    match x with
    | ' ' -> true
    | _ -> false
  in
  lift
    (fun str -> name_c (String.concat "" [ "\""; str; "\"" ]))
    (char '\"' *> take_while is_non_quote <* char '\"')
;;

let number =
  let is_num = function
    | '0' .. '9' -> true
    | _ -> false
  in
  lay_seq *> take_while1 is_num >>| int_of_string >>| num_c
;;

let atom = name <|> (string <|> (token "[]" >>| name_c))
let atomic = atom >>| atom_c <|> number

let variable =
  let anon_var = token "_" >>| var_c in
  let named_var =
    lift2
      (fun hd tl -> Var (String.make 1 hd ^ tl))
      (satisfy (function
        | 'A' .. 'Z' | '_' -> true
        | _ -> false))
      (take_while is_alphanumeric)
  in
  lay_seq *> (anon_var <|> named_var)
;;

let compound =
  fix (fun compound ->
    let create_Compound atom terms = Compound { atom; terms } in
    let atomic = lift (fun x -> Atomic x) atomic in
    let term_with_prefix_op =
      let create_Compound op term = create_Compound op [ term ] in
      lift2 create_Compound prefix_operator compound
    in
    let term_in_func_notation =
      let args = parens (sep_by1 comma compound) in
      lift2 create_Compound atom args
    in
    let term_in_list_notation =
      fix (fun term_in_list_notation ->
        let list_atom = Atomic (Atom (Name "[]")) in
        let list_c r1 r2 = Compound { atom = Name "."; terms = [ r1; r2 ] } in
        let term =
          term_in_list_notation
          <|> term_with_prefix_op
          <|> term_in_func_notation
          <|> atomic
          <|> variable
        in
        let items =
          fix (fun items ->
            lift2 list_c (term <* comma) items
            <|> (lift2 list_c (term <* bar) term
                <|> lift (fun r -> list_c r list_atom) term))
        in
        brackets items)
    in
    rnd_brackets
      (chainl1
         (chainl1 (chainl1 compound (infix_operator 700)) (infix_operator 1000))
         (infix_operator 1200))
    <|> term_in_list_notation
    <|> term_with_prefix_op
    <|> term_in_func_notation
    <|> atomic
    <|> variable)
;;

let term =
  chainl1
    (chainl1 (chainl1 compound (infix_operator 700)) (infix_operator 1000))
    (infix_operator 1200)
  <* dot
;;

let parse_prolog = many term

let parse_program str =
  match parse_string ~consume:Consume.All parse_prolog str with
  | Ok x -> Ok x
  | Error er -> Error (`ParsingError er)
;;

let parse_query str =
  match parse_string ~consume:Consume.All term str with
  | Ok x -> Ok x
  | Error er -> Error (`ParsingError er)
;;
