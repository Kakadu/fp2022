(** Copyright 2021-2022, Startsev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type 'a parse_rez = Parsed of 'a | Failed of string

val parse : string -> (ast, string) result
val eval : string -> ast parse_rez
