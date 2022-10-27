(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Pascal language parser. *)
val parse : string -> Ast.t option
