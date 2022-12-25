(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving show { with_path = false }]

type const =
  | Bool of bool
  | Int of int
  | Unit
[@@deriving show { with_path = false }]

type bin_op =
  | Plus
  | Minus
  | Mult
  | Divide
  | Mod
  | Eq
  | Neq
  | Lt
  | Ltq
  | Gt
  | Gtq
[@@deriving show { with_path = false }]

type arg_label =
  | ArgNoLabel
  | ArgLabeled of id
  | ArgOptional of id
[@@deriving show { with_path = false }]

type expr =
  | Const of const
  | Var of id
  | Binop of bin_op * expr * expr
  | Fun of arg_label * expr option * id * expr
  | App of expr * arg_label * expr
  | IfThenElse of expr * expr * expr
  | Let of id * expr * expr
  | LetRec of id * expr * expr
[@@deriving show { with_path = false }]

type definition = id * expr [@@deriving show { with_path = false }]

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
  | Arrow of typ * typ
