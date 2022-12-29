(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type atom =
  | Name of string
  | Operator of string
[@@deriving eq, show { with_path = false }]

type atomic =
  | Num of int
  | Atom of atom
[@@deriving eq, show { with_path = false }]

type term =
  | Atomic of atomic
  | Var of string
  | Compound of
      { atom : atom
      ; terms : term list
      }
[@@deriving eq, show { with_path = false }]
