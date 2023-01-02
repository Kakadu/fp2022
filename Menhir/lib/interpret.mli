(** Copyright 2021-2022, Artur Gagin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

exception NoSeparator of string

val read_all_file_text : string -> string
val split_string_on_spaces : string -> string list
val parse' : string -> string list * string * Ast.grammar

exception RejectApplyingRule
exception OvershootApplyingRule

val get_parser_and_tree_printer
  :  string
  -> (string list -> Ast.parse_tree list) * (Ast.parse_tree list -> string)
