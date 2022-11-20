(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom

let token_separator = take_while (fun x -> Char.equal x ' ')
let as_token p = token_separator *> p <* token_separator

let expr_separator =
  take_while1 (fun x -> Char.equal x '\n' || Char.equal x ';')
  >>| (fun _ -> ";")
  |> as_token
;;

let new_lines = take_while1 (fun x -> Char.equal x '\n') >>| (fun _ -> "\n") |> as_token

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let integer_t = take_while1 is_digit |> as_token
let true_t = string "true" |> as_token
let false_t = string "false" |> as_token
let bool_t = true_t <|> false_t

let ruby_string =
  char '"' *> take_while (fun c -> not (Char.equal c '"')) <* char '"' |> as_token
;;

let asoc0_t = choice [ string "&&"; string "*"; string "/" ] |> as_token
let asoc1_t = choice [ string "||"; string "+"; string "-" ] |> as_token

let asoc2_t =
  choice [ string "=="; string "!="; string ">="; string "<="; string ">"; string "<" ]
  |> as_token
;;

let binops = choice [ asoc0_t; asoc1_t; asoc2_t ]

let is_letter = function
  | 'a' .. 'z' -> true
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_letter_or_und c = is_letter c || c = '_'
let keywords = [ "if"; "then"; "else"; "end"; "true"; "false"; "while"; "do"; "def" ]

let identifier_t =
  take_while1 is_letter_or_und
  >>= (fun s1 ->
        take_while (fun c -> is_letter_or_und c || is_digit c) >>| fun s2 -> s1 ^ s2)
  |> as_token
  >>= fun i ->
  match List.find_opt (String.equal i) keywords with
  | Some _ -> fail "Keyword can't be identifier"
  | None -> return i
;;

let token s = as_token (string s)
