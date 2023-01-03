(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type state

val empty_state : state
val get_variable : state -> string -> Ast.value
val set_local_var : state -> string -> Ast.value -> state
