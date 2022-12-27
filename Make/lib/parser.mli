(** Copyright 2021-2022, ol-imorozko and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Main entry of parser *)
val parse : string -> (Ast.ast, string) result
