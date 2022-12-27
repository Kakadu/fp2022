(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
open Ast

open Interpret
open Prettyprint

let commands =
  [ "Usage:"
  ; " #help         Prints a list of all available commands"
  ; " #quit         Exit the toplevel loop and terminate this program"
  ; " #use <file>   Read and evaluate source phrases from the given file"
  ]
;;

let run_repl = Format.eprintf "OCaml-style toplevel (ocamlc, utop) is not implemented"

(* let rec repl env toplevel =
  let module E = Interpret (EvalResult) in
  try
    match toplevel with
    | Definition (name, exp) ->
      let value = E.eval exp env in
      (match value with
       | Result.Ok v ->
         Format.printf "val %s : <type_undef> = %a\n" name pp_value v;
         v
       | Result.Error _ ->
         Format.printf "Error: definition evaluation failure \n%!";
         VUndef)
    | Expression exp ->
      let value = E.eval exp env in
      (match value with
       | Result.Ok v -> Format.printf "- : <type_undef> = %a\n" pp_value v
       | Result.Error _ -> Format.printf "Error: expression evaluation failure \n%!");
      repl env free_vars
    | Command c ->
      (match c with
       | Help ->
         List.iter Format.pp_print_string commands;
         repl env free_vars
       | Quit ->
         Format.printf "Quiting...";
         exit 0
       | Use file -> load file)
  with
  | ParseError s ->
    Format.printf "Parse_Error: %s\n" s;
    env free_vars
  | TypeError s ->
    Format.printf "Type_Error: %s\n" s;
    env free_vars
  | RuntimeError s ->
    Format.printf "Runtime_Error: %s\n" s;
    env free_vars
  | _ ->
    Format.printf "Error: reseting environment\n";
    repl IdMap.empty free_vars
;;

let rec load file =
  try
    let definitions = Stdio.In_channel.(read_all file) |> String.rstrip in
    let free_vars = Caml.Hashtbl.create 16 in
    let add_def env (id, e) =
      let value = eval_def id e env free_vars in
      IdMap.add id (ref value) env
    in
    let env = List.fold_left (add_def IdMap.empty definitions) in
    repl env free_vars
  with
  | Failure msg ->
    print_endline msg;
    exit 1
  | _ -> exit 1
;; *)

let rec repl env toplevel =
  let module E = Interpret (EvalResult) in
  match toplevel with
  | Definition (name, exp) ->
    let value = E.eval exp env in
    (match value with
     | Result.Ok v -> Format.printf "val %s : <type_undef> = %a\n" name pp_value v
     | Result.Error _ -> Format.printf "Error: definition evaluation failure \n%!")
  | Expression exp ->
    let value = E.eval exp env in
    (match value with
     | Result.Ok v -> Format.printf "- : <type_undef> = %a\n" pp_value v
     | Result.Error _ -> Format.printf "Error: expression evaluation failure \n%!")
  | Command c ->
    (match c with
     | Help -> List.iter (Format.printf "%s") commands
     | Quit ->
       Format.printf "Quiting...";
       exit 0
     | Use file -> Format.printf "#Use file not implemented")
;;

let run_single () =
  Format.printf "(mini_repl) | OCaml subset with labeled arguments #\n";
  let text = Stdio.In_channel.(input_all stdin) |> Base.String.rstrip in
  match Parser.parse_toplevel text with
  | Error e -> Format.printf "%s" e
  | Result.Ok input -> repl IdMap.empty input
;;
