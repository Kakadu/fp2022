(** Copyright 2021-2022, Denis Porsev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parsetree

module IdMap = Map.Make (struct
  type t = id

  let compare = compare
end)

type value =
  | VUndef
  | VUnit
  | VBool of bool
  | VInt of int
  | VClosure of value ref IdMap.t * arg_label * expr option * id * expr

type environment = value ref IdMap.t

type typ =
  | TBool
  | TInt
  | TUnit
  | TVar of id
  | Arrow of typ * arg_label * typ