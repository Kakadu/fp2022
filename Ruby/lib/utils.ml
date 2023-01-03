(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

let rec string_of_value = function
  | Bool v -> string_of_bool v
  | Integer v -> string_of_int v
  | String v -> v
  | Nil -> "nil"
  | Array l ->
    String.concat "" [ "["; List.map string_of_value !l |> String.concat ", "; "]" ]
  | Function (name, params, _) ->
    String.concat "" [ name; "("; String.concat ", " params; ")" ]
;;

let value_of_literal (lit_t : ruby_literal) (s : string) =
  match lit_t with
  | BoolL -> Bool (bool_of_string s)
  | IntegerL -> Integer (int_of_string s)
  | StringL -> String s
  | NilL -> Nil
;;

let typefail msg = failwith ("TypeError: " ^ msg)
