(** Copyright 2021-2022, Arthur Alekseev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Ast = struct
  type ident = string [@@deriving show { with_path = false }]

  type unop =
    | Not
    | USub
  [@@deriving show { with_path = false }]

  type logic_binop =
    | And
    | Or
  [@@deriving show { with_path = false }]

  type arithm_binop =
    | Add
    | Mul
    | Sub
    | Div
    | Pow
  [@@deriving show { with_path = false }]

  type compare_binop =
    | Le
    | Ge
    | Lt
    | Gt
    | Eq
    | Ne
  [@@deriving show { with_path = false }]

  type string_binop = Concat [@@deriving show { with_path = false }]

  type binop =
    | LOp of logic_binop
    | AOp of arithm_binop
    | COp of compare_binop
    | SOp of string_binop
  [@@deriving show { with_path = false }]

  type block = statement list [@@deriving show { with_path = false }]

  and expr_table_entry =
    | JustExpr of expr
    | PairExpr of expr * expr

  and l_function = ident list * block

  and const =
    | Bool of bool
    | Number of float
    | String of string
    | Function of l_function
    | Nil
  [@@deriving show { with_path = false }]

  and expr =
    | Const of const
    | Variable of ident
    | TableGet of expr * expr
    | TableInit of expr_table_entry list
    | BinOp of binop * expr * expr
    | UnOp of unop * expr
    | ExprApply of apply
  [@@deriving show { with_path = false }]

  and apply = Call of expr * expr list [@@deriving show { with_path = false }]

  and statement =
    | Do of block
    | Set of lvalue list * expr list
    | While of expr * block
    | Repeat of block * expr
    | If of expr * block * elseif_block list * block option
    | Fornum of ident * expr * expr * expr option * block
    | Forin of ident list * expr list * block
    | Local of ident list * expr list
    | Return of expr option
    | Break
    | StatementApply of apply
    | FunctionDeclare of lvalue * ident list * block
    | Expr of expr
  [@@deriving show { with_path = false }]

  and lvalue =
    | Index of lvalue * expr
    | Ident of ident
  [@@deriving show { with_path = false }]

  and elseif_block = expr * block

  type ast = statement list [@@deriving show { with_path = false }]
  type t = ast

  let show_const x = Format.asprintf "%a" pp_const x
end
