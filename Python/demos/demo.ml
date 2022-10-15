(** Copyright 2021-2022, Evgeniy Bakaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Python_lib.Interpreter
open Eval (Result)

let () =
  let path = String.trim (Stdio.In_channel.input_all Caml.stdin) in
  let ic = open_in path in
  try
    let code = Stdio.In_channel.input_all ic in
    let y = parse_and_interpet code in
    match y with
    | Ok x -> print_endline x
    | Error x ->
      print_endline x;
      close_in ic
  with
  | e ->
    close_in ic;
    raise e
;;
