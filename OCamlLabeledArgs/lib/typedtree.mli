(** Copyright 2022-2023, Denis Porsev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** The typed AST representation of our mini language. *)

(** Typed tree *)

open Parsetree

(** Mapping from variable names to values *)
module IdMap : Map.S with type key = id

(** Possible output values of expressions *)
type value =
  | VUndef (** internall undefined value *)
  | VUnit (** internall value for () *)
  | VBool of bool (** represents boolean values *)
  | VInt of int (** represents integer values *)
  | VClosure of id option * environment * expr
      (** represents high-order functions in form of (optional name for recursion, env, Fun (...) ) *)

(** Mapping from variable names to types *)
and environment = value IdMap.t

(** Representation of types *)
type typ =
  | TBool (** boolean type *)
  | TInt (** integer type *)
  | TUnit (** unit type *)
  | TVar of id (** representation of type variable 'id, needed for polymorphism *)
  | Arrow of typ * arg_label * typ (** representation of function types *)
