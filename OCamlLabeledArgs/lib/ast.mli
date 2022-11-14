(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** The AST representation of our mini language *)

(** Built-in string type for variable names *)
type id = string

(** Type for constants *)
type const =
  | Bool of bool (** true or false *)
  | Int of int (** integer literal *)
  | Nil (** empty list [] *)
  | Unit (** value ()  FIXME: Do i need it?*)

(** Enumeration of all available binary operators *)
type bin_op =
  | Plus (** + operator *)
  | Minus (** - operator *)
  | Mult (** * operator *)
  | Divide (** / operator *)
  | Mod (** mod (remainder) operator *)
  | Eq (** = operator *)
  | Neq (** != operator *)
  | Lt (** < operator *)
  | Ltq (** <= operator *)
  | Gt (** > operator *)
  | Gtq (** >= operator *)

(** Type for expressions *)
type expr =
  | Const of const
  | Var of id
  | Binop of bin_op * expr * expr
  | Fun of id * expr (* Only one argument => Parser should handle multiple args*)
  | Cons of expr * expr (* FIXME: Do i need it? *)
  | If of expr * expr * expr
  | Let of id * expr * expr
  | LetRec of id * expr * expr

(** Representation of function arguments *)
type arg_label =
  | NoLabel
  | Labelled of string
  | Optional of string

(** Representation of types *)
type typ =
  | TBool (** boolean type *)
  | TInt (** integer type *)
  | TUnit (** unit type *)
  | TVar of id (** representation of type variable 'name, needed for polymorphism *)
  | TList of typ (** representation of typ list *)
  | Arrow of typ * typ (** representation of function types *)
