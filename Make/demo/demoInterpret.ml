(** Copyright 2021-2022, Morozko Ivan *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Make_lib
open Core

let _ =
  Stdlib.Sys.chdir "test_project";
  let () =
    let input = In_channel.read_all "./Makefile" in
    match Parser.parse input with
    | Result.Ok ast ->
      Format.printf "%a\n%!\n" Ast.pp_ast ast;
      (match Interpret.interpret ast [] with
       | Result.Ok str -> print_string str
       | Error str -> print_string str)
    | Error m -> Format.printf "Parse error \n%s!" m
  in
  ()
;;
