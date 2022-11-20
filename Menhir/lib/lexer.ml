(** Copyright 2021-2022, Artur Gagin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Sedlexing.Utf8
open Parser

exception InvalidToken of string

let whitespace = [%sedlex.regexp? Plus (' ' | '\n' | '\t')]
let token_char = [%sedlex.regexp? 'A' .. 'Z']
let token = [%sedlex.regexp? Plus token_char]
let nonterm_char = [%sedlex.regexp? 'a' .. 'z']
let nonterm = [%sedlex.regexp? Plus nonterm_char]
let rule_comp_char = [%sedlex.regexp? 'A' .. 'Z' | 'a' .. 'z']
let rule_comp = [%sedlex.regexp? Plus rule_comp_char]

let rec tokenizer buf =
  match%sedlex buf with
  | whitespace -> tokenizer buf
  | "%token", whitespace, token ->
    TOKEN (Str.replace_first (Str.regexp "%token[\n\t ]+") "" (lexeme buf))
  | "%start", whitespace, nonterm ->
    START (Str.replace_first (Str.regexp "%start[\n\t ]+") "" (lexeme buf))
  | "%%" -> PROCENTPROCENT
  | '|' -> VERT
  | nonterm, ':' -> NONTERM (Str.replace_first (Str.regexp ":") "" (lexeme buf))
  | rule_comp -> RULECOMPONENT (lexeme buf)
  | ';' -> SEMICOLON
  | eof -> EOF
  | _ -> assert false
;;

let provider buf () =
  let token = tokenizer buf in
  let start, stop = Sedlexing.lexing_positions buf in
  token, start, stop
;;

let from_string f string =
  provider (from_string string) |> MenhirLib.Convert.Simplified.traditional2revised f
;;
