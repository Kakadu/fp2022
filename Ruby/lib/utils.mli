(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val string_of_value : Ast.value -> string
val value_of_literal : Ast.ruby_literal -> string -> Ast.value
