(** Copyright 2021-2022, Ilya Shchuckin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error = [ `ParsingError of string ]

val pp_error : Format.formatter -> [< `ParsingError of string ] -> unit

(** Main parsers *)

val parse_query : string -> (Ast.term, error) result
val parse_program : string -> (Ast.term list, error) result

(** Miniparsers *)

val name : Ast.atom Angstrom.t
val term : Ast.term Angstrom.t
val variable : Ast.term Angstrom.t
val atom : Ast.atom Angstrom.t
val atomic : Ast.atomic Angstrom.t
val number : Ast.atomic Angstrom.t
val string : Ast.atom Angstrom.t
