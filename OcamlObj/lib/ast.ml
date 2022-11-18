(** Copyright 2021-2022, Kalashnikov Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type const =
  | CInt of int
  | CString of string
  | CBool of bool
[@@deriving eq, show { with_path = false }]

type binop =
  | Add (**  +   *)
  | Sub (**  -   *)
  | Mul (**  *   *)
  | Div (**  /   *)
  | Less (**  <   *)
  | Leq (**  <=  *)
  | Gre (**  >   *)
  | Geq (**  >=  *)
  | Eq (** ==  *)
  | Neq (** <>  *)
  | And (** &&  *)
  | Or (** ||  *)
[@@deriving eq, show { with_path = false }]

type name = string [@@deriving eq, show { with_path = false }]

type expr =
  | EUnit
  | EConst of const
  | EBinop of binop * expr * expr
  | EVar of name
  | EIf of expr * expr * expr
  | ELet of decl * expr
  | EFun of pattern * expr
  | EApp of expr * expr
  | EMatch of expr * (pattern * expr) list
  | ECallM of name * name
  | EObj of obj
[@@deriving eq, show { with_path = false }]

and objexpr =
  | OMeth of pattern * expr
  | OVal of pattern * expr
[@@deriving eq, show { with_path = false }]

and obj = objexpr list [@@deriving eq, show { with_path = false }]

and pattern =
  | PUnit
  | PVar of name
  | PConst of const
[@@deriving eq, show { with_path = false }]

and decl = bool * pattern * expr [@@deriving eq, show { with_path = false }]
and declaration = DLet of decl [@@deriving eq, show { with_path = false }]

type prog = declaration list [@@deriving eq, show { with_path = false }]
