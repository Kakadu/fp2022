(** Copyright 2021-2022, Kakadu, EmirVildanov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Pretty printer module *)

open Ast

exception PrinterException of string

val type_to_string : Typing.t -> string
val binop_to_string : binary_op -> string
val unop_to_string : unary_op -> string
val expr_to_string : expr -> string
val val_to_string : value -> string
