(** Copyright 2021-2022, Startsev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Asm.Interpreter
open Asm.Parser
open Interpret (Result)

let run_str env code =
  match eval code with
  | Parsed ast -> (
      match interpret env ast with
      | Ok env ->
          print_endline @@ show_envr env;
          env
      | Error msg ->
          print_endline msg;
          env)
  | Failed msg ->
      print_endline msg;
      env

let rec run_repl env =
  print_string "~> ";
  run_repl @@ run_str env (read_line ())

let () =
  print_endline "Welcome to ASM REPL!";
  run_repl @@ prep r_list