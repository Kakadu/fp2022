(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** The AST representation of our mini language *)

(* ---------- Parse tree ----------*)

(** Built-in string type for variable names *)
type id = string

(** Type for constants *)
type const =
  | Bool of bool (** true or false *)
  | Int of int (** integer literal *)
  | Unit (** () *)

(** Enumeration of all available binary operators *)
type bin_op =
  | Plus (** + operator *)
  | Minus (** - operator *)
  | Mult (** * operator *)
  | Divide (** / operator *)
  | Mod (** mod (remainder) operator *)
  | Eq (** = operator *)
  | Neq (** <> operator *)
  | Lt (** < operator *)
  | Ltq (** <= operator *)
  | Gt (** > operator *)
  | Gtq (** >= operator *)

(** Representation of function arguments *)
type arg_label =
  | ArgNoLabel
  | ArgLabeled of id
  | ArgOptional of id

(** Type for expressions *)
type expr =
  | Const of const (** Constant expressions *)
  | Var of id (** Variable names expressions *)
  | Binop of bin_op * expr * expr (** Expressions involving binary operations *)
  (* For anonymous functions "fun a b -> e" is syntactic sugar, 
     which parser has to resolve to "Fun ("x", Fun ("y", e))" *)
  | Fun of arg_label * expr option * id * expr (** Anonymous functions *)
  | App of expr * arg_label * expr (** Function application *)
  | IfThenElse of expr * expr * expr (** Conditional operator if-then-else *)
  (* Expressions like "let f x = x" are handled by parser,
     which produces definition Let (Var "f", Fun ("x", Var "x")) *)
  | Let of id * expr * expr (** Expression for "let x = e1 in e2" *)
  | LetRec of id * expr * expr (** Fixpointed Let *)

(** Type for definitions *)
type definition = id * expr [@@deriving show { with_path = false }]

(* ---------- Typed tree ----------*)

(** Mapping from variable names to values *)
module IdMap : Map.S with type key = id

(** Possible output values of expressions *)
type value =
  | VUndef (** internall value for implementing rec *)
  | VUnit (** internall value for () *)
  | VBool of bool (** represents boolean values *)
  | VInt of int (** represents integer values *)
  | VClosure of value Stdlib.ref IdMap.t * arg_label * expr option * id * expr
      (** represents high-order functions in form of (env, "x", e) *)

(** Mapping from variable names to types *)
type environment = value Stdlib.ref IdMap.t

(** Representation of types *)
type typ =
  | TBool (** boolean type *)
  | TInt (** integer type *)
  | TUnit (** unit type *)
  | TVar of id (** representation of type variable 'id, needed for polymorphism *)
  | Arrow of typ * typ (** representation of function types *)
