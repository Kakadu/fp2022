(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Ident

val lookup : string source_file -> (ident source_file, string list) result
