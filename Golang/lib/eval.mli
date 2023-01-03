(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Ident

val eval : ident source_file -> (unit, string) result
