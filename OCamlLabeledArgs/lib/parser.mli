(** Copyright 2022-2023, Denis Porsev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** [Parser] is the front-end part of the interpreter. *)

(** Basic parser combinators used in testing *)

val ignored : unit Angstrom.t
val identifier : string Angstrom.t
val label_parser : Parsetree.arg_label Angstrom.t
val expr_parser : Parsetree.expr Angstrom.t
val definition_parser : Parsetree.definition Angstrom.t

(** [parse] is responsible for parsing user input with specific parser ['a Angstrom.t]. *)
val parse : 'a Angstrom.t -> string -> ('a, string) result

(** [parse_toplvevel] is the entry point of a parser. It should only be called on top-level ast type. *)
val parse_toplevel : string -> (Parsetree.toplevel list, string) result