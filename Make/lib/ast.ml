(** Copyright 2021-2022, ol-imorozko and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base

type recipe =
  | Echo of string
  | Silent of string
[@@deriving show { with_path = false }, variants, sexp]

(* <target> [<target[s]>...]: [<prerequisite[s]>...]
    [<recipe[s]>...]
 *)
type rule =
  { targets : string * string list
  ; prerequisites : string list
  ; recipes : recipe list
  }
[@@deriving show { with_path = false }]

type expr = Rule of rule [@@deriving show { with_path = false }]

(* Makefile should contain at least one rule *)
type ast = rule * expr list [@@deriving show { with_path = false }]
