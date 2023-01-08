(** Copyright 2021-2022, Kakadu, EmirVildanov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type error = [ `ParsingError of string ]

val pp_error : Format.formatter -> [< `ParsingError of string ] -> unit

(** Main entry of parser *)
val parse : string -> (expr, error) result
