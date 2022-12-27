(** Copyright 2021-2022, Arthur Alekseev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Parser : sig
  type input

  type 'a parse_result =
    | Failed of string
    | Parsed of 'a * input
    | HardFailed of string
  (* HardFailed is used in case the error in parsing is severe, like a forgotten closing bracket. It indicates an error to be given back from parser*)

  type 'a parser

  val string_to_input : string -> input
  val input_to_string : input -> string
  val parse : input -> Ast.Ast.ast parse_result
  val parse_expr : input -> Ast.Ast.expr parse_result
end
