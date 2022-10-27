(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open KeyMap
open ImArray

type name = string

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | FDiv
  | And
  | Or
  | Xor
  | Greater
  | Less
  | Eq
  | NotEq
  | GreaterEq
  | LessEq
  | RShift
  | LShift
[@@deriving show]

type unop = Plus | Minus | Not [@@deriving show]

type vtype =
  | VTBool
  | VTInt
  | VTFloat
  | VTChar
  | VTString of expr
  | VTNDString
  | VTDString of int
  | VTRecord of (name * vtype) list
  | VTDRecord of vtype KeyMap.t
  | VTFunction of fun_param list * vtype
  | VTArray of expr * expr * vtype
  | VTDArray of value * int * vtype
  | VTCustom of name
  | VTCollable of name
  | VTVoid
[@@deriving show]

and value =
  | VBool of bool
  | VInt of int
  | VFloat of float
  | VChar of char
  | VString of string
  | VRecord of world
  | VFunction of name * vtype * fun_param list * world * statement list
  | VArray of value * int * vtype * value ImArray.t
  | VCollable of name
  | VVoid
[@@deriving show]

and fun_param =
  | FPFree of name * vtype
  | FPOut of name * vtype
  | FPConst of name * vtype
[@@deriving show]

and expr =
  | Const of value
  | Variable of name
  | BinOp of binop * expr * expr
  | UnOp of unop * expr
  | Call of expr * expr list
  | GetRec of expr * name
  | GetArr of expr * expr
[@@deriving show]

and statement =
  | Assign of expr * expr
  | ProcCall of expr
  | If of expr * statement list * statement list
  | While of expr * statement list
  | Repeat of expr * statement list
  | For of name * expr * expr * statement list
  | Break
  | Continue
[@@deriving show]

and define =
  | DType of name * vtype
  | DNDVariable of name * vtype
  | DVariable of name * vtype * expr
  | DDVariable of name * vtype * value
  | DConst of name * expr
  | DDConst of name * value
  | DFunction of name * vtype * fun_param list * t
[@@deriving show]

and variable = VConst of value | VVariable of value | VType [@@deriving show]

and world = (vtype * variable) KeyMap.t [@@deriving show]

and t = define list * statement list [@@deriving show]
