(** Copyright 2021-2022, Chizhov Anton *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Bash_lib

let () =
  let input = Stdio.In_channel.input_all Caml.stdin in
  match Parser.parse input with
  | Result.Ok ast -> Format.printf "%a\n%!" Ast.pp_script ast
  | Error m -> Format.printf "Parse error %a\n%!" Parser.pp_error m
;;
