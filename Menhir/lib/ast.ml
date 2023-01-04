(** Copyright 2021-2022, Artur Gagin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type grammar = string * (string * string list) list

type parse_tree =
  | Term of string
  | Nonterm of string * parse_tree list
