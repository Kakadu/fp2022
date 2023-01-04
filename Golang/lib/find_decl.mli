(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Ident

type found_decl =
  | Var of ident var_decl
  | Func of ident func_decl
  | Arg of ident arg

val find : ident source_file -> ident -> found_decl option
