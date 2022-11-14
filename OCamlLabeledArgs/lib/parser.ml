(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

(* ------------------------------------------------------ *)
(* -------------------- Basic syntax -------------------- *)
(* ------------------------------------------------------ *)

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

(* space, horizontal tabulation, carriage return, line feed, form feed *)
let is_whitespace = function
  | '\x20' | '\x09' | '\x0d' | '\x0a' | '\x0c' -> true
  | _ -> false
;;

let whitespace = take_while is_whitespace
let token s = whitespace *> string s

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let between a b p = a *> p <* b
let pstring s = between whitespace whitespace (string s)
let parens p = between (pstring "(") (pstring ")") p

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let identifier =
  let is_valid_first_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | _ -> false
  in
  let is_valid_id = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  in
  whitespace *> peek_char
  >>= function
  | Some c when is_valid_first_char c ->
    take_while is_valid_id
    >>= fun s ->
    (match is_keyword s with
     | false -> return s
     | true -> fail "Invalid variable name")
  | _ -> fail "Invalid variable name"
;;

(* -------------------- Const parser -------------------- *)

let int_literal = take_while1 is_digit

let integer =
  option "" (string "+" <|> string "-")
  >>= fun sign -> int_literal >>= fun whole -> return (int_of_string (sign ^ whole))
;;

(* -------------------- Variable -------------------- *)

(* ------------------- Operators -------------------- *)
let plus = token "+" *> return Plus

(* ------------------------------------------------------ *)
(* ------------------ Top-level parser ------------------ *)
(* ------------------------------------------------------ *)
let parse p s = parse_string ~consume:All p s