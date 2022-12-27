(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

val match_binop : string -> value -> value -> value
val conditional : value -> ast -> ast -> ast
val indexing : value -> value -> value
