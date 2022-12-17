(** Copyright 2021-2022, Artur Gagin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Menhir_lib

let read_command () =
  try read_line () with
  | End_of_file -> ""
;;

let rec repl_tokens_list parser tree_parser =
  let () =
    print_string "> ";
    let command = read_command () in
    if String.equal command "exit"
    then exit 0
    else (
      let input = Interpret.split_string_and_delete_spaces command in
      let res, ret = parser input in
      if res
      then print_endline ("ACCEPT\n" ^ tree_parser input)
      else if ret = 0
      then print_endline "REJECT"
      else print_endline "OVERSHOOT")
  in
  repl_tokens_list parser tree_parser
;;

let rec repl_command ret =
  print_string "$ ";
  let () =
    let command = read_command () in
    match Interpret.get_parser_and_tree_parser command with
    | Ok (parser, tree_parser) -> repl_tokens_list parser tree_parser
    | Error e -> print_endline e
  in
  repl_command ret
;;

let () =
  print_endline "\n\t Menhir Interpreter REPL";
  print_endline "\tAuthor: @lastdesire";
  print_endline "\tTutor: @Kakadu";
  print_endline
    "> To upload your grammar type 'menhir --interpret \
     <PATH-TO-YOUR-MLY-FILE-WHICH-CONTAINS-GRAMMAR>' and press Enter;\n\
     > Then you can write sentences and parser will try to interprete it:";
  print_endline " * ACCEPT: your sentence was successfully parsed;";
  print_endline
    " * OVERSHOOT: the end of your sentence was reached before it could be accepted";
  print_endline " * REJECT: the sentence was not accepted;";
  print_endline
    "Menhir Reference Manual: https://gallium.inria.fr/~fpottier/menhir/manual.html";
  print_endline "> To exit you can type 'exit'.";
  repl_command 0
;;
