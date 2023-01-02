(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Ast types for literals *)
type ruby_literal =
  | BoolL (** Bool literal*)
  | IntegerL (** Integer literal *)
  | StringL (** String literal *)
  | NilL (** Nil literal *)

(** Ast types used in parsing*)
type ast =
  | Literal of ruby_literal * string (** Literal [literal_type literal_as_string] *)
  | ArrayDecl of ast list (** ArrayDecl [initial_content] *)
  | Var of string (** Var [var_name] *)
  | VarAssign of string * ast (** VarAssign [var_name new_value] *)
  | Conditional of ast * ast * ast (** Conditional [condition then_branch else_branch] *)
  | WhileLoop of ast * ast (** WhileLoop [condition body] *)
  | Binop of string * ast * ast (** Binop [op left right] *)
  | Seq of ast list (** Seq [expressions] *)
  | Indexing of ast * ast (** Indexing [box index] *)
  | FuncDeclaration of string * string list * ast
      (** FunctionDeclaration [name param_names body]*)
  | Invocation of ast * ast list (** Invocation [name param_values] *)
  | MethodAccess of ast * string (** MethodAccess [object method]*)

(** Data types used in runtime *)
type value =
  | Bool of bool (** Bool [value]*)
  | Integer of int (** Integer [value]*)
  | String of string (** String [value]*)
  | Array of value list (** Array [value_list]*)
  | Function of string * string list * (value list -> value)
      (** Function [name param_list body]*)
  | Nil (** Nil *)
