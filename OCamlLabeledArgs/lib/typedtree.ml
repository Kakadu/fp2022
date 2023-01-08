(** Copyright 2022-2023, Denis Porsev and contributors *)

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
  | VClosure of id option * value IdMap.t * expr

type environment = value IdMap.t

type typ =
  | TBool
  | TInt
  | TUnit
  | TVar of id
  | Arrow of typ * arg_label * typ
