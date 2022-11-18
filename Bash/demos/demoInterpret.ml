(** Copyright 2021-2022, Chizhov Anton *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Bash_lib
open Parser

let interpret script =
  let open Interpret.Interpret (Interpret.Result) in
  match eval script with
  | Ok env -> pp_environment Format.std_formatter env
  | Error e -> Printf.printf "Interpretation error: %s" e
;;

let parse_and_interpret s =
  match Parser.parse s with
  | Ok script ->
    Format.printf "%a\n%!" Ast.pp_script script;
    interpret script
  | Error e -> pp_error Format.std_formatter e
;;

let () = parse_and_interpret (Stdio.In_channel.input_all stdin)
