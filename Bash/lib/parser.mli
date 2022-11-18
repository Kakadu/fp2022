(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error = [ `ParsingError of string ]

val pp_error : Format.formatter -> [< `ParsingError of string ] -> unit

(** Main entry of parser *)
val parse : string -> (Ast.script, error) result

(* A collection of miniparsers *)
val parse_script : unit -> Ast.script Angstrom.t
