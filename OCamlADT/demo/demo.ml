(** Copyright 2021-2022, Kakadu, EmirVildanov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamladt_lib

exception CommandNotFound

let _ =
  let code = Stdio.In_channel.input_all Caml.stdin in
  try
    let semicolons_reg = Str.regexp ";;" in
    let _ = Str.search_forward semicolons_reg code 0 in
    let final_string_variants = Str.split semicolons_reg code in
    let final_string =
      match final_string_variants with
      | [] -> raise CommandNotFound
      | [ x ] -> x
      | h :: _ -> h
    in
    let ast = Parser.parse final_string in
    match ast with
    | Result.Ok result ->
      let type_checking_result = Infer.run_inference result in
      (match type_checking_result with
       | Error error ->
         Infer.print_type_error error;
         let eval_res = Interpreter.eval result Ast.IdMap.empty in
         Format.printf "%s : fail\n" (Printer.val_to_string eval_res.value)
       | Ok t ->
         let eval_res = Interpreter.eval result Ast.IdMap.empty in
         Format.printf
           "%s : %s\n"
           (Printer.val_to_string eval_res.value)
           (Printer.type_to_string t))
    | Result.Error e ->
      Caml.Format.printf "Error on parsing state: %a\n%!" Parser.pp_error e
  with
  | Not_found -> print_endline "No string!"
  | CommandNotFound -> prerr_endline "Internal error during command substring search!"
;;
