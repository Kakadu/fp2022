(** Copyright 2021-2022, Artur Gagin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Menhir_lib

let read_command () =
  try read_line () with
  | End_of_file -> ""
;;

let rec repl_tokens_list parser tree_parser =
  let () =
    print_string "$ ";
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

(* Arg module usage *)
let interpret = ref false
let input_file = ref ""
let usage = "usage: " ^ Sys.argv.(0) ^ " menhir-interpret <file>"

let speclist =
  [ ( "menhir-interpret"
    , Arg.Set interpret
    , ": necessary flag responsible for starting the interpreter" )
  ]
;;

let anon_fun x = input_file := x

let check_errors () =
  if Array.length Sys.argv <> 3
  then
    raise (Arg.Bad "Bad number of arguments: use 'ocaml REPL.ml --help' for information.")
  else ();
  let first_arg = Sys.argv.(1) in
  if String.equal "menhir-interpret" first_arg
  then ()
  else raise (Arg.Bad ("Bad arg (first arg should be 'menhir-interpret'): " ^ first_arg))
;;

exception OpenFileError of string
exception ParseProcessError of string

let () =
  (* Read the arguments *)
  print_endline
    "\n>>>>>>>>>>>>>>>>>>>>>>>>>Menhir interpreter REPL<<<<<<<<<<<<<<<<<<<<<<<<<";
  Arg.parse speclist anon_fun usage;
  check_errors ();
  try
    let text = Interpret.read_all_file_text (Unix.openfile !input_file [] 0) in
    match Interpret.get_parser_and_tree_parser text with
    | Ok (parser, tree_parser) ->
      print_endline "\tAuthor: @lastdesire";
      print_endline "\tTutor: @Kakadu";
      print_endline "> Write sentences and parser will try to interprete it:";
      print_endline " * ACCEPT: your sentence was successfully parsed;";
      print_endline
        " * OVERSHOOT: the end of your sentence was reached before it could be accepted";
      print_endline " * REJECT: the sentence was not accepted;";
      print_endline
        "> Menhir Reference Manual: https://gallium.inria.fr/~fpottier/menhir/manual.html";
      print_endline "> To exit you can type 'exit'.";
      repl_tokens_list parser tree_parser
    | Error e -> raise (ParseProcessError e)
  with
  | Unix.Unix_error _ ->
    raise
      (OpenFileError ("Please, check path on correctness, can't open file: " ^ !input_file))
;;
