(** Copyright 2021-2022, Evgeniy Bakaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Python_lib.Interpreter
open Eval (Result)

let node_class =
  "class Node:\n\
   \tdef init(v):\n\
   \t\tself.value = v\n\
   \tdef get():\n\
   \t\treturn self.value\n\
   node1 = Node()\n\
   node1.init(5)\n\
   node2 = Node()\n\
   node2.init(10)\n\
   [node1.get(), node2.get()]"
;;

let test =
  match parse_and_interpet node_class with
  | Ok x -> print_endline x
  | Error x -> print_endline x
;;
