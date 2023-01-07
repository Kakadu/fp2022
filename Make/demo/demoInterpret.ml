(** Copyright 2021-2022, Morozko Ivan *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Make_lib

let _ =
  let argv = Sys.argv |> Array.to_list in
  let argv = List.tl argv in
  (* Stdlib.Sys.chdir (List.hd argv); *)
  let () =
    let input = Core.In_channel.read_all "./Makefile" in
    match Parser.parse input with
    | Result.Ok ast ->
      (* Format.printf "%a\n%!\n" Ast.pp_ast ast; *)
      (match Interpret.interpret ast argv with
       | Result.Ok str -> print_string str
       | Error str -> print_string str)
    | Error m -> Format.printf "Parse error \n%s!" m
  in
  ()
;;
