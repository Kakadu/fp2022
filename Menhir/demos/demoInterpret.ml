(** Copyright 2021-2022, Artur Gagin *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Menhir_lib

let read_command () =
  try read_line () with
  | End_of_file -> ""
;;

let () =
  let path = read_command () in
  let text = Interpret.read_all_file_text path in
  let parser, tree_printer =
    try Interpret.get_parser_and_tree_printer text with
    | Interpret.NoSeparator _ | Lexer.InvalidToken _ | Parser.Error ->
      print_endline "Some error in parse part";
      (* These errors we check in demoParse.ml *)
      exit 1
  in
  print_endline "The file was successfully parsed.";
  let token_list = String.split_on_char ' ' (read_command ()) in
  try print_endline (tree_printer (parser token_list)) with
  | Interpret.RejectApplyingRule -> print_endline "REJECT"
  | Interpret.OvershootApplyingRule -> print_endline "OVERSHOOT"
;;
