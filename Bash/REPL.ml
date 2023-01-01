(** Copyright 2021-2022, Chizhov Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Bash_lib
open Ast
open Interpret.Interpret (Interpret.Result)

type mode =
  | DEBUG
  | RELEASE

type opts = { mutable mode : mode }

let rec run_single ~opts ?(env = default_env) () =
  let input = Stdio.In_channel.(input_all stdin) |> String.rstrip in
  match Parser.parse input with
  | Error e -> Caml.Format.printf "Parsing error: %a\n%!" Parser.pp_error e
  | Result.Ok ast ->
    (match opts.mode with
     | DEBUG -> Caml.Format.printf "Parsed result: %a\n%!" pp_script ast
     | _ -> ());
    (match eval ~env ast with
     | Ok env ->
       (match opts.mode with
        | DEBUG -> Caml.Format.printf "Evaluated result: %a\n%!" pp_environment env
        | _ -> ());
       run_single ~opts ~env ()
     | Error e -> Caml.Format.printf "Interpretation error: %s" e)
;;

let () =
  let opts = { mode = RELEASE } in
  let open Caml.Arg in
  parse
    [ "-debug", Unit (fun () -> opts.mode <- DEBUG), "Start bash in debug mode" ]
    (fun _ ->
      Caml.Format.eprintf "Positioned arguments are not supported\n";
      Caml.exit 1)
    "Read-Eval-Print-Loop for Bash inerpreter";
  run_single ~opts ()
;;
