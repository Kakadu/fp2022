(** Copyright 2021-2022, Artur Gagin *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Menhir_lib

let read_command () =
  try read_line () with
  | End_of_file -> ""
;;

let rec print_grammar = function
  | (lhs, rhs) :: tl ->
    Format.printf "%s: " lhs;
    List.iter (fun x -> Format.printf "%s; " x) rhs;
    Format.printf "\n";
    print_grammar tl
  | _ -> ()
;;

let () =
  let path = read_command () in
  let text = Interpret.read_all_file_text path in
  let tokens, start_rule, grammar =
    try Menhir_lib.Interpret.parse' text with
    | Interpret.NoSeparator s ->
      print_endline s;
      exit 1
    | Lexer.InvalidToken (l, s) ->
      Format.printf "InvalidToken error: line %s at %s" l s;
      exit 1
  in
  Format.printf "List of tokens: ";
  List.iter (fun x -> Format.printf "%s " x) tokens;
  Format.printf "\nStart rule: %s" start_rule;
  let _, g' = grammar in
  Format.printf "\nGrammar:\n";
  print_grammar g'
;;
