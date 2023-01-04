(** Copyright 2021-2022, Startsev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Asm.Interpreter
open Asm.Parser
open Asm.Ast
open Interpret (Result)
open Int64

let run_str env st code =
  match eval code with
  | Parsed (Ast ast) -> (
      match interpret env st ast with
      | Ok (env, st, _) ->
          print_endline @@ show_ast (Ast ast);
          List.iter print_endline (List.map to_string st);
          print_endline @@ show_envr env;
          (env, st)
      | Error msg ->
          print_endline @@ show_ast (Ast ast);
          List.iter print_endline (List.map to_string st);
          print_endline msg;
          (env, st))
  | Failed msg ->
      print_endline msg;
      (env, st)

let rec run_repl env st =
  match run_str env st (Stdio.In_channel.input_all Caml.stdin) with
  | env, st -> run_repl env st

let () =
  print_endline "Welcome to ASM REPL!";
  run_repl (prep r_list) []
