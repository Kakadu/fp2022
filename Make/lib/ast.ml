(** Copyright 2021-2022, ol-imorozko and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base

(* <target> [<target[s]>...]: [<prerequisite[s]>...]
    [<recipe[s]>...]
 *)
type rule =
  { targets : string * string list
  ; prerequisites : string list
  ; recipes : string list
  }
[@@deriving show { with_path = false }]

type expr = Rule of rule [@@deriving show { with_path = false }]

(* Makefile should contain at least one rule *)
type ast = rule * expr list [@@deriving show { with_path = false }]
