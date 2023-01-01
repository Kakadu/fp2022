(** Copyright 2022-2023, Denis Porsev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** The AST representation of our mini language *)

(** Parse tree *)

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

(** Top-level tree *)

(** Top-level input *)
type command =
  | Help
  | Quit
  | Use of string

type toplevel =
  | Definition of definition
  | Expression of expr
  | Command of command
