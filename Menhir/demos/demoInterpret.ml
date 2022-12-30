(** Copyright 2021-2022, Artur Gagin *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Menhir_lib

let read_command () =
  try read_line () with
  | End_of_file -> ""
;;

let () =
  let path = read_command () in
  let text = Interpret.read_all_file_text (Unix.openfile path [] 0) in
  let parser, tree_parser =
    try Interpret.get_parser_and_tree_parser text with
    | _ ->
      print_endline "Some error in parse part";
      exit 1
  in
  print_endline "The file was successfully parsed.";
  let token_list = String.split_on_char ' ' (read_command ()) in
  let b, ret = parser token_list in
  if b
  then print_endline (tree_parser token_list)
  else if ret = 0
  then print_endline "REJECT"
  else print_endline "OVERSHOOT"
;;
