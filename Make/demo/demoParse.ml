(** Copyright 2021-2022, Morozko Ivan *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Make_lib

let () =
  let input = Stdio.In_channel.input_all Caml.stdin in
  match Parser.parse input with
  | Result.Ok ast -> Format.printf "%a\n%!\n" Ast.pp_ast ast
  | Error m -> Format.printf "Parse error \n%s!" m
;;
