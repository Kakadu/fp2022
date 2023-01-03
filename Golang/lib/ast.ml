(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type typ =
  | IntTyp (* integer type *)
  | ArrayTyp of array_typ (* array type *)
  | FunTyp of string signature (* function type *)
  | ChanTyp of typ (* chan T *)
  | StrTyp (* string type *)
  | BoolTyp (* boolean type *)
[@@deriving show, ord]

and array_typ = { el : typ }

and 'id signature =
  { args : 'id arg list
  ; ret : return_typ
  }

and 'id arg = 'id * typ

and return_typ =
  | One of typ (* function returns one value *)
  | Void (* returns void *)

(* Operators *)

type unaryop =
  | Minus (* -expr *)
  | Not (* !expr *)
  | Receive (* <- chan *)
[@@deriving show]

type binop =
  | Mul (* * *)
  | Div (* / *)
  | Mod (* % *)
  | Add (* + *)
  | Sub (* - *)
  | Eq (* == *)
  | Neq (* != *)
  | Lt (* < *)
  | Lte (* <= *)
  | Gt (* > *)
  | Gte (* >= *)
  | And (* && *)
  | Or (* || *)
[@@deriving show]

(* Literals *)
type constant =
  | Int of int (* integer literals *)
  | Str of string (* string literals *)
  | Bool of bool (* boolean literals *)
[@@deriving show]

(* Nodes *)

type 'id expr =
  | Const of constant (* literal constants *)
  | Ident of 'id (* identifiers *)
  | ArrLit of (array_typ * 'id expr list) (* [n]type {e1, e2, ..., en} *)
  | ArrIndex of ('id expr * 'id expr) (* arr[i] *)
  | Call of ('id expr * 'id expr list) (* f(arg1, arg2, ... argn) *)
  | FuncLit of ('id signature * 'id block) (* func( params ) return_type { ... } *)
  | UnOp of (unaryop * 'id expr) (* unop expr *)
  | BinOp of ('id expr * binop * 'id expr) (* expr binop expr *)
  | Print of 'id expr list (* print(arg1, arg2, ... argn) *)
  | Len of 'id expr (* len(arr) *)
  | Append of 'id expr * 'id expr list (* append(arr, x, y, z, ...) *)
  | Make of typ
[@@deriving show]

and 'id var_decl = 'id * 'id expr
and 'id func_decl = 'id * 'id signature * 'id block

and 'id top_level_decl =
  | GlobalVarDecl of 'id var_decl (* Global var a = expr; *)
  | FuncDecl of 'id func_decl (* func f(params) return_type { ... } *)
[@@deriving show]

and 'id block = 'id stmt list [@@deriving show, ord]

and 'id stmt =
  | AssignStmt of 'id expr * 'id expr (* a = 1 and b[i] = 1 *)
  | VarDecl of 'id var_decl (* var a = expr *)
  | BlockStmt of 'id block (* { ... } *)
  | ExprStmt of 'id expr (* expr; *)
  | GoStmt of 'id expr (* go f(); *)
  | RetStmt of 'id expr option (* return expr; *)
  | IfStmt of 'id expr * 'id block * 'id block (* if expr { } else { } *)
  | ForStmt of 'id expr * 'id block (* for expr { } *)
  | SendStmt of 'id expr * 'id expr (* chan <- x *)
[@@deriving show, ord]

type 'id source_file = 'id top_level_decl list [@@deriving show]
(* A parsed source file *)
