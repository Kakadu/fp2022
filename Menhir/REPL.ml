(** Copyright 2021-2022, Artur Gagin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Menhir_lib

let read_command () =
  try read_line () with
  | End_of_file -> ""
;;

let rec repl_tokens_list parser tree_printer =
  let () =
    print_string "$ ";
    let command = read_command () in
    if String.equal command "exit"
    then exit 0
    else (
      let input = Interpret.split_string_on_spaces command in
      try print_endline (tree_printer (parser input)) with
      | Interpret.RejectApplyingRule -> print_endline "REJECT"
      | Interpret.OvershootApplyingRule -> print_endline "OVERSHOOT")
  in
  repl_tokens_list parser tree_printer
;;

(* Arg module usage *)
let interpret = ref false
let input_file = ref ""
let usage = Format.sprintf "usage: %s menhir-interpret <file>" Sys.argv.(0)

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
      {|USAGE: dune exec ./REPL.exe menhir-interpret <path-to-file>
       YOUR FILE SHOULD HAVE A SYNTAX SIMILAR TO THE FOLLOWING EXAMPLE:
        /* USE ONLY UPPERCASE LETTERS IN NONTERM NAMES */
        /* USE ONLY LOWERCASE LETTERS IN TERM NAMES */
        /* FOLLOW THIS EXAMPLE */
          %token TOKEN_1
          ...
          %token TOKEN_n
          %start starttoken
          %%
          starttoken: /* nonterm_1 */
          | <TOKEN_i/nonterm_j>; ...; <TOKEN_i/nonterm_j>
          ...
          | ...
          ...
          nonterm_k:
          | ...
          ...
          | ...|};
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
  let text =
    try Interpret.read_all_file_text !input_file with
    | Sys_error _ ->
      raise
        (OpenFileError
           ("Please, check path on correctness, can't open file: " ^ !input_file))
  in
  let parser, tree_printer =
    try Interpret.get_parser_and_tree_printer text with
    | Lexer.InvalidToken (l, s) ->
      raise
        (ParseProcessError
           (Format.sprintf
              "Lexer Error: line %s at: %s. You can use command 'dune exec ./REPL.exe \
               help' for get more information about required syntax."
              l
              s))
    (* Error from lexer. *)
    | Parser.Error ->
      (* Error from parser. *)
      raise
        (ParseProcessError
           "Parse Error: make sure you write nonterms with lowercase letters only and \
            terms with uppercase only (don't use any other symbols). You can use command \
            'dune exec ./REPL.exe help' for get more information about required syntax.")
      (* Only in this situation we have parse error, in other case there is InvalidToken exception. *)
    | Interpret.NoSeparator s -> raise (ParseProcessError s)
  in
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
  repl_tokens_list parser tree_printer
;;
