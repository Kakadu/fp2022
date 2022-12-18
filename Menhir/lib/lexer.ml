(** Copyright 2021-2022, Artur Gagin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Sedlexing.Utf8
open Parser

(* line * str *)
exception InvalidToken of string * string

let whitespace = [%sedlex.regexp? Plus (' ' | '\t')]
let whitespace' = [%sedlex.regexp? Plus (' ' | '\n' | '\t')]
let any_char = [%sedlex.regexp? any]
let new_line = [%sedlex.regexp? '\n']
let token_char = [%sedlex.regexp? 'A' .. 'Z' | '_']
let token = [%sedlex.regexp? Plus token_char]
let nonterm_char = [%sedlex.regexp? 'a' .. 'z' | '_']
let nonterm = [%sedlex.regexp? Plus nonterm_char]
let rule_comp_char = [%sedlex.regexp? 'A' .. 'Z' | 'a' .. 'z' | '_']
let rule_comp = [%sedlex.regexp? Plus rule_comp_char]

type error_lexeme_type =
  { mutable line_pos : int
  ; mutable str : string
  }

let error_lexeme : error_lexeme_type = { line_pos = 1; str = "" }

let rec take_while_no_whitespace (buf : Sedlexing.lexbuf) =
  match%sedlex buf with
  | whitespace' -> ()
  | any_char ->
    error_lexeme.str <- error_lexeme.str ^ lexeme buf;
    take_while_no_whitespace buf
  | _ -> ()
;;

let rec tokenizer
  (buf : Sedlexing.lexbuf)
  (start : Lexing.position)
  (stop : Lexing.position)
  =
  match%sedlex buf with
  | whitespace -> tokenizer buf start stop
  | new_line ->
    error_lexeme.line_pos <- error_lexeme.line_pos + 1;
    tokenizer buf start stop
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
  | _ ->
    let line = error_lexeme.line_pos in
    error_lexeme.line_pos <- 1;
    let () = take_while_no_whitespace buf in
    let str = error_lexeme.str in
    error_lexeme.str <- "";
    raise (InvalidToken (string_of_int line, str))
;;

let provider buf () =
  let start, stop = Sedlexing.lexing_positions buf in
  let token = tokenizer buf start stop in
  token, start, stop
;;

let from_string f string =
  provider (from_string string) |> MenhirLib.Convert.Simplified.traditional2revised f
;;
