(** Copyright 2021-2022, Kakadu, EmirVildanov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamladt_lib

exception CommandNotFound

let rec read_next_command previous =
  let new_input = read_line () in
  let concatted = previous ^ new_input in
  let semicolons_reg = Str.regexp ";;" in
  try
    let _ = Str.search_forward semicolons_reg concatted 0 in
    let final_string = Str.split semicolons_reg concatted in
    match final_string with
    | [] -> raise CommandNotFound
    | [ x ] -> x
    | h :: _ -> h
  with
  | Not_found -> read_next_command concatted
  | CommandNotFound -> read_next_command ""
;;

let rec eval_new_repl_command env =
  print_string "# ";
  let input_string = read_next_command "" in
  let ast = Parser.parse input_string in
  match ast with
  | Result.Ok result ->
    let type_checking_result = Infer.run_inference result in
    (match type_checking_result with
     | Error error ->
       Infer.print_type_error error;
       eval_new_repl_command env
     | Ok t ->
       let eval_res = Interpreter.eval result env in
       Format.printf
         "%s %s\n"
         (Printer.val_to_string eval_res.value)
         (Printer.type_to_string t);
       eval_new_repl_command eval_res.env)
  | Result.Error _ ->
    print_string "Error on parsing stage.\n";
    eval_new_repl_command env
;;

let start_repl = eval_new_repl_command Ast.IdMap.empty
