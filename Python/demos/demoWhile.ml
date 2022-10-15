(** Copyright 2021-2022, Evgeniy Bakaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Python_lib.Interpreter
open Eval (Result)

let prog = "x = 0\nwhile x < 10:\n\tx = x + 1\nx"

let test =
  match parse_and_interpet prog with
  | Ok x -> print_endline x
  | Error x -> print_endline x
;;
