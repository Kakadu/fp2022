(** Copyright 2021-2022, Danila Pechenev & Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Interpret.InterpretResult
open InterpreterPP
open Inferencer
open Parser
open Format
open Typing

let print_run code =
  match parse code with
  | Ok ast ->
    (match R.run (check_types ast) with
     | Ok _ ->
       (match run ast with
        | Ok result -> print_value result
        | Error error -> print_error error)
     | Error error -> print_type_error error)
  | Error error -> printf "%s\n" error
;;
