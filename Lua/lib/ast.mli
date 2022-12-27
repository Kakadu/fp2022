(** Copyright 2021-2022, Arthur Alekseev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Ast : sig
  type ident = string [@@deriving show { with_path = false }]

  type unop =
    | Not (** not *)
    | USub (** - *)
  [@@deriving show { with_path = false }]

  type logic_binop =
    | And (** and *)
    | Or (** or *)
  [@@deriving show { with_path = false }]

  type arithm_binop =
    | Add (** + *)
    | Mul (** * *)
    | Sub (** - *)
    | Div (** / *)
    | Pow (** ^ *)
  [@@deriving show { with_path = false }]

  type compare_binop =
    | Le (** <= *)
    | Ge (** >= *)
    | Lt (** > *)
    | Gt (** < *)
    | Eq (** == *)
    | Ne (** ~= *)
  [@@deriving show { with_path = false }]

  type string_binop = Concat (** .. *) [@@deriving show { with_path = false }]

  type binop =
    | LOp of logic_binop (** Logic operators: and, or*)
    | AOp of arithm_binop (** Arithmetic operators: +. -. /, *, ^ *)
    | COp of compare_binop (** Comparative operators: <=, >=, <, >, ==, ~= *)
    | SOp of string_binop (** String operators: .. *)
  [@@deriving show { with_path = false }]

  type block = statement list [@@deriving show { with_path = false }]

  and expr_table_entry =
    | JustExpr of expr
        (** Just an expressinon, its index will be determined by position *)
    | PairExpr of expr * expr (** Key/Value Pair ([key]=value)*)

  and l_function = ident list * block

  and const =
    | Bool of bool (** true | false *)
    | Number of float (** 1, 2.3, 3, ...*)
    | String of string (** "abc" *)
    | Function of l_function (** function(args) block end *)
    | Nil (** nil *)
  [@@deriving show { with_path = false }, ord]

  and expr =
    | Const of const (** number or string or ..., written in const *)
    | Variable of ident (** obv. variable *)
    | TableGet of expr * expr (** expr[expr] *)
    | TableInit of expr_table_entry list (** {expr, expr, expr, ...} *)
    | BinOp of binop * expr * expr (** expr binop expr *)
    | UnOp of unop * expr (** unop expr*)
    | ExprApply of apply (** expr(expr, ..., expr) *)
  [@@deriving show { with_path = false }]

  and apply = Call of expr * expr list (** expr(expr, ..., expr) *)
  [@@deriving show { with_path = false }]

  and statement =
    | Do of block (** do sttmn sttmnt end*)
    | Set of lvalue list * expr list (** a, b, c = d, e, f*)
    | While of expr * block (** while expr do block*)
    | Repeat of block * expr (** repeat block until expr *)
    | If of expr * block * elseif_block list * block option
        (** if expr then block [elseif block] else block end*)
    | Fornum of ident * expr * expr * expr option * block
        (** for var = expr, expr, ?expr, block *)
    | Forin of ident list * expr list * block
        (** for expr, expr, ... in expr, expr, ... do block *)
    | Local of ident list * expr list (** local a = ?expr *)
    | Return of expr option (** return expr *)
    | Break (** break *)
    | StatementApply of apply (** just a fun call, needed because of side effects  *)
    | FunctionDeclare of lvalue * ident list * block (** function name(args) body end*)
    | Expr of expr (** just an expression, will be printed, for REPL *)
  [@@deriving show { with_path = false }]

  and lvalue =
    | Index of lvalue * expr (** a[1][2][3]..[n][n][n] *)
    | Ident of ident (** var *)
  [@@deriving show { with_path = false }]

  (** elseif expr then block *)
  and elseif_block = expr * block

  type ast = block [@@deriving show { with_path = false }]
  type t = ast
end
