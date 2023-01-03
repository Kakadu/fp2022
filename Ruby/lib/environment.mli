(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

val empty_state : state
val empty_class_state : class_state
val from_global : state -> state
val get_variable : state -> string -> value
val set_local_var : state -> string -> value -> state
val set_class_var : state -> string -> value -> state
val add_class_scope : state -> class_state ref -> state
val set_in_class_state : class_state -> string -> value -> class_state
val get_from_class_state : class_state -> string -> value
