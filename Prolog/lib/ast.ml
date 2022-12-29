(** Copyright 2021-2022, Ilya Shchuckin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Defines AST for prolog programs (6.2; 6.3 in ISO) *)

(** 6.3.1.3 in ISO*)
type atom =
  | Name of string
  | Operator of string
[@@deriving eq, show { with_path = false }]

(** 6.3.1 in ISO *)
type atomic =
  | Num of int
  | Atom of atom
[@@deriving eq, show { with_path = false }]

(** 6.3 in ISO *)
type term =
  | Atomic of atomic
  | Var of string
  | Compound of
      { atom : atom
      ; terms : term list
      }
[@@deriving eq, show { with_path = false }]
