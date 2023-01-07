(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Ast types for literals *)
open Base

type ruby_literal =
  | BoolL (** Bool literal*)
  | IntegerL (** Integer literal *)
  | StringL (** String literal *)
  | NilL (** Nil literal *)

(** Function scope *)
type func_scope =
  | Method (** ClassMethod *)
  | Lambda (** Anonymous function *)

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
  | FuncDeclaration of func_scope * string * string list * ast
      (** FunctionDeclaration [func_scope name param_names body]*)
  | Invocation of ast * ast list (** Invocation [target param_values] *)
  | MethodAccess of ast * string * ast list (** MethodAccess [object method params]*)
  | ClassDeclaration of string * ast list (** ClassDeclaration [name members]*)

(** Data types used in runtime *)
type value =
  | Bool of bool (** Bool [value]*)
  | Integer of int (** Integer [value]*)
  | String of string (** String [value]*)
  | Array of value list (** Array [value_list]*)
  | Function of string * string list * ast (** Function [name param_list body]*)
  (* Lambda need it's own constructor because it inherits state from outer scope when it is declared*)
  | Lambda of state * string list * ast (** Lambda [closure param_list * body]*)
  | Class of class_state (** Class [initial_state] *)
  | ClassInstance of class_state (** ClassInstance [shared_instance_state] *)
  | Nil (** Nil *)

and class_state = (string, value, String.comparator_witness) Map.t

and state =
  { local_vars : (string, value, String.comparator_witness) Map.t
  ; class_scopes : class_state list
  }
