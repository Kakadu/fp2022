(** Copyright 2022-2023, Denis Porsev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
open Parsetree

open Typedtree
open Interpret
open Interpret (EvalResult)
open Prettyprint

let commands =
  [ "Usage:"
  ; " #help         Prints a list of all available commands"
  ; " #quit         Exit the toplevel loop and terminate this program"
  ; " #use <file>   Read and evaluate source phrases from the given file"
  ]
;;

let repl env toplevel =
  match toplevel with
  | Definition (name, exp) ->
    let res_value = eval exp env in
    let value =
      match res_value with
      | Result.Ok v -> v
      | Result.Error e ->
        Prettyprint.pp_error Format.err_formatter e;
        VUndef
    in
    let env = IdMap.add name (ref value) env in
    (match res_value with
     | Result.Ok v -> Format.printf "val %s : <type_undef> = %a\n" name pp_value v
     | Result.Error _ -> Format.printf "Error: definition evaluation failure \n%!");
    env
  | Expression exp ->
    let value = eval exp env in
    (match value with
     | Result.Ok v -> Format.printf "- : <type_undef> = %a\n" pp_value v
     | Result.Error e -> pp_error Format.std_formatter e);
    env
  | Command c ->
    (match c with
     | Help -> List.iter (Format.printf "%s\n") commands
     | Quit ->
       Format.printf "Quiting...";
       exit 0
     (* TODO: implement #use *)
     | Use file -> Format.printf "#Use file not implemented for now. Your file: %s" file);
    env
;;

let init_repl is_debug input =
  match Parser.parse_toplevel input with
  | Error e -> Format.printf "\nError: %s\n" e
  | Result.Ok toplevel_input ->
    let rec helper env toplevel_input =
      match toplevel_input with
      | [] -> IdMap.empty
      | [ h ] -> repl env h
      | h :: tl -> helper (repl env h) tl
    in
    if is_debug
    then Prettyprint.pp_env Format.std_formatter (helper IdMap.empty toplevel_input)
    else (
      let _ = helper IdMap.empty toplevel_input in
      ())
;;

let run_repl is_debug =
  (* Format.printf "(mini_repl) | OCaml subset with labeled arguments #\n"; *)
  let rec cycle_repl () =
    let text = read_line () in
    match text with
    | exception End_of_file -> ()
    | exception Failure msg -> Format.printf "Exception: %s" msg
    | _ -> cycle_repl (init_repl is_debug text)
  in
  cycle_repl
;;

let run_single is_debug =
  (* Format.printf "(mini_repl) | OCaml subset with labeled arguments #\n"; *)
  let text = Stdio.In_channel.(input_all stdin) |> Base.String.rstrip in
  init_repl is_debug text
;;
