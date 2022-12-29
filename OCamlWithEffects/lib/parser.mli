(** Copyright 2021-2022, Danila Pechenev & Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error_message = string
type input = string

val parse : input -> (Ast.expression list, error_message) result
