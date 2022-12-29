(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error = [ `ParsingError of string ]

val pp_error : Format.formatter -> [< `ParsingError of string ] -> unit

(** Main entry of parser *)
val parse_query : string -> (Ast.term, error) result

val parse_program : string -> (Ast.term list, error) result

(* A collection of miniparsers *)
val name : Ast.atom Angstrom.t
val term : Ast.term Angstrom.t
