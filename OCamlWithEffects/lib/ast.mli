(** Copyright 2021-2022, Danila Pechenev & Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typing

type capitalized_id = string [@@deriving eq, show { with_path = false }]
type id = string [@@deriving eq, show { with_path = false }]
type data_constructor_name = string [@@deriving eq, show { with_path = false }]

type literal =
  | LInt of int (** 42 *)
  | LString of string (** "42" *)
  | LChar of char (** '\n' *)
  | LBool of bool (** true *)
  | LUnit (** () *)
[@@deriving eq, show { with_path = false }]

type binary_operator =
  | Add (** + *)
  | Sub (** - *)
  | Mul (** * *)
  | Div (** / *)
  | Eq (** = *)
  | NEq (** != *)
  | GT (** > *)
  | GTE (** >= *)
  | LT (** < *)
  | LTE (** <= *)
  | AND (** && *)
  | OR (** || *)
[@@deriving show { with_path = false }]

type unary_operator =
  | Minus (** -1 *)
  | Not (** not true *)
[@@deriving show { with_path = false }]

type expression =
  | ELiteral of literal (** 123 *)
  | EBinaryOperation of binary_operator * expression * expression (** 1 + 3 *)
  | EUnaryOperation of unary_operator * expression (** -(1 + 3) *)
  | EApplication of expression * expression (** f x *)
  | EIdentifier of id (** x *)
  | EFun of id list * expression (** fun x y -> x + y *)
  | EList of expression list (** [ 1; 2; 3 ] *)
  | EConstructList of expression * expression (** 1 :: [2; 3] *)
  | ETuple of expression list (** (1, "Vasya Pupkin", '\n') *)
  | EDeclaration of id * id list * expression (** let add x y = x + y *)
  | ERecursiveDeclaration of id * id list * expression
      (** let rec factorial n = n * factorial (n - 1) *)
  | ELetIn of expression list * expression (** let x = 1 and y = 2 in x + y *)
  | EIf of expression * expression * expression (** if true then 1 else 0 *)
  | EMatchWith of expression * (expression * expression) list (** match x with _ -> x *)
  | EDataConstructor of data_constructor_name * expression option (** Some 5 *)
  | EEffectDeclaration of id * typ (** effect E : int -> string effect *)
  | EEffectNoArg of capitalized_id (** E *)
  | EEffectArg of capitalized_id * expression (** E 1 *)
  | EPerform of expression (** perform (E 1) *)
  | EContinue of expression (** continue 3 *)
  | EEffectPattern of expression (** effect (Failure _) -> *)
