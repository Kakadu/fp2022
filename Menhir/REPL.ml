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
      let input = Interpret.split_string_on_spaces command in
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
let usage = String.concat "" [ "usage: "; Sys.argv.(0); " menhir-interpret <file>" ]

let speclist =
  [ ( "menhir-interpret"
    , Arg.Set interpret
    , ": necessary flag responsible for starting the interpreter" )
  ]
;;

let anon_fun x = input_file := x

let check_errors () =
  match Sys.argv with
  | [| _; "menhir-interpret"; _ |] -> ()
  | _ ->
    raise
      (Arg.Bad
         "Bad args: use dune exec ./REPL.exe help to get information about correct usage")
;;

let print_help () =
  match Sys.argv with
  | [| _; "help" |] ->
    print_endline
      "USAGE: dune exec ./REPL.exe menhir-interpret <path-to-file>\n\n\
       YOUR FILE SHOULD HAVE A SYNTAX SIMILAR TO THE FOLLOWING EXAMPLE:\n\
       \t /* USE ONLY UPPERCASE LETTERS IN NONTERM NAMES */\n\
       \t /* USE ONLY LOWERCASE LETTERS IN TERM NAMES */\n\
       \t /* FOLLOW THIS EXAMPLE */\n\
       \t %token TOKEN_1\n\
       \t ...\n\
       \t %token TOKEN_n\n\
       \t %start starttoken\n\n\
       \t %%\n\n\
       \t starttoken: /* nonterm_1 */\n\
       \t | <TOKEN_i/nonterm_j>; ...; <TOKEN_i/nonterm_j>\n\
       \t ...\n\
       \t | ...\n\n\
       \t ...\n\n\
       \t nonterm_k:\n\
       \t | ...\n\
       \t ...\n\
       \t | ...\n";
    exit 0
  | _ -> ()
;;

exception OpenFileError of string
exception ParseProcessError of string

let () =
  (* Read the arguments *)
  print_endline
    "\n>>>>>>>>>>>>>>>>>>>>>>>>>Menhir interpreter REPL<<<<<<<<<<<<<<<<<<<<<<<<<";
  Arg.parse speclist anon_fun usage;
  print_help ();
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
