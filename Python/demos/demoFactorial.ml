(** Copyright 2021-2022, Evgeniy Bakaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Python_lib.Interpreter
open Eval (Result)

let factorial =
  "\n\n\
   def fact(n):\n\
   \tif n < 0:\n\
   \t\treturn\n\
   \tif n == 0 or n == 1:\n\
   \t\treturn 1\n\
   \treturn n * fact(n-1)\n\
   [fact(1), fact(2), fact(3), fact(4),fact(5)]\n\n"
;;

let test =
  match parse_and_interpet factorial with
  | Ok x -> print_endline x
  | Error x -> print_endline x
;;
