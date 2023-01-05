(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base

let rec string_of_value = function
  | Bool v -> Bool.to_string v
  | Integer v -> Int.to_string v
  | String v -> v
  | Nil -> "nil"
  | Array l ->
    String.concat [ "["; List.map ~f:string_of_value l |> String.concat ~sep:", "; "]" ]
  | Function (name, params, _) ->
    String.concat [ name; "("; String.concat ~sep:", " params; ")" ]
  | Class class_state ->
    String.concat ~sep:" " ([ "<"; "Class" ] @ print_state class_state @ [ ">" ])
  | ClassInstance ref_class_state ->
    String.concat
      ~sep:" "
      ([ "<"; "ClassInstance" ] @ print_state ref_class_state @ [ ">" ])
  | Lambda (_, params, _) ->
    String.concat [ "Lambda"; "("; String.concat ~sep:", " params; ")" ]

and print_state st =
  st
  |> Map.to_alist
  |> List.map ~f:(fun (k, v) -> String.concat [ k; "="; string_of_value v ])
;;

let value_of_literal lit_t s =
  match lit_t with
  | BoolL -> Bool (Bool.of_string s)
  | IntegerL -> Integer (Int.of_string s)
  | StringL -> String s
  | NilL -> Nil
;;
