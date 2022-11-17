(** Copyright 2021-2022, Startsev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val parse : string -> (Ast.ast list, string) result
val eval : string -> Ast.ast list