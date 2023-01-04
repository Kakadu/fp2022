(** Copyright 2021-2022, Artur Gagin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

exception InvalidToken of string * string

val from_string : (Parser.token, 'a) MenhirLib.Convert.traditional -> string -> 'a
