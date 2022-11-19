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
    let input = Interpret.split_string_and_delete_spaces command in
    let res, ret = parser input in
    if res
    then print_endline ("ACCEPT\n" ^ tree_parser input)
    else if ret = 0
    then print_endline "REJECT"
    else print_endline "OVERSHOOT"
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
  print_endline "\t Menhir Interpreter REPL";
  print_endline
    "> To upload your grammar type 'menhir --interpret \
     <PATH-TO-YOUR-MLY-FILE-WHICH-CONTAINS-GRAMMAR>' and press Enter;\n\
     \t> Then you can write sentences and parser will try to interprete it;";
  print_endline
    "> If you want to get a parser, which one can parse this grammar, type 'menhir \
     --parser <PATH-TO-YOUR-MLY-FILE-WHICH-CONTAINS-GRAMMAR>' and press Enter;\n\
     \t> Note that this parser will return true or false depending on whether it can \
     parse your sentence or not;\n\
     \t> If you want to get a parser which will return parseTree then type menhir \
     --treeParser <PATH-TO-YOUR-MLY-FILE-WHICH-CONTAINS-GRAMMAR>;";
  print_endline "> To exit you can type 'exit'.";
  repl_command 0
;;
