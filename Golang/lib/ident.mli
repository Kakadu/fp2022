(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base

type ident =
  { name : string
  ; scope : int
  }

val error_ident : ident
val ident : scope:int -> string -> ident
val name : ident -> string
val eq_ident : ident -> ident -> bool
val show_ident : ident -> string

type 'v tbl

val set : 'v tbl -> key:ident -> data:'v -> 'v tbl
val find : 'v tbl -> ident -> 'v option
val empty_tbl : 'v tbl
