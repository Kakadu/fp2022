(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open KeyMap
open ImArray

type name = string

(** binary operators *)
type binop =
  | Add (** ( + ) *)
  | Sub (** ( - ) *)
  | Mul (** ( * ) *)
  | Div (** ( div ) *)
  | Mod (** ( mod ) *)
  | FDiv (** ( / ) *)
  | And (** ( and ) *)
  | Or (** ( or ) *)
  | Xor (** ( xor ) *)
  | Greater (** ( > ) *)
  | Less (** ( < ) *)
  | Eq (** ( = ) *)
  | NotEq (** ( <> ) *)
  | GreaterEq (** ( >= ) *)
  | LessEq (** ( <= ) *)
  | RShift (** ( bit shift right ) *)
  | LShift (** ( bit shift left ) *)
[@@deriving show]

(** unar operators *)
type unop =
  | Plus (** + *)
  | Minus (** - *)
  | Not (** not *)
[@@deriving show]

(** virtual types *)
type vtype =
  | VTBool (** boolean type *)
  | VTInt (** integer type *)
  | VTFloat (** real type *)
  | VTChar (** char type *)
  | VTString of expr (** string type with expr len *)
  | VTNDString (** string type without len, will be used 255 *)
  | VTDString of int (** string type with len *)
  | VTRecord of (name * vtype) list (** record type of definitions *)
  | VTDRecord of vtype KeyMap.t (** record type constructed *)
  | VTFunction of fun_param list * vtype (** function type *)
  | VTArray of expr * expr * vtype (** array type with expr..expr interval *)
  | VTDArray of value * int * vtype
      (** array type with calculated interval : (from <val> with <size> of <type>) *)
  | VTCustom of name (** custom type *)
  | VTCollable of name (** special type for std function name before call *)
  | VTVoid (** special type for procedure declaration *)
[@@deriving show]

(** virtual values *)
and value =
  | VBool of bool (** boolean value *)
  | VInt of int (** integer value *)
  | VFloat of float (** real value *)
  | VChar of char (** char value *)
  | VString of string (** string value *)
  | VRecord of world (** record value *)
  | VFunction of name * vtype * fun_param list * world * statement list
      (** function value : (<name> <result type> <param list> <local world> <realization>) *)
  | VArray of value * int * vtype * value ImArray.t
      (** array value : from <val> with <size> of <type>, contains <arr>*)
  | VCollable of name (** special value for std function name before call *)
  | VVoid
      (** special type for procedure declaration or for an unassigned function variable *)
[@@deriving show]

(** function parameters *)
and fun_param =
  | FPFree of name * vtype (** standard parameter *)
  | FPOut of name * vtype (** out parameter *)
  | FPConst of name * vtype (** const parameter *)
[@@deriving show]

(** expression *)
and expr =
  | Const of value (** const *)
  | Variable of name (** variable *)
  | BinOp of binop * expr * expr (** binary operator *)
  | UnOp of unop * expr (** unary operator *)
  | Call of expr * expr list (** function calling *)
  | GetRec of expr * name (** record field getting *)
  | GetArr of expr * expr (** array field getting *)
[@@deriving show]

(** statement *)
and statement =
  | Assign of expr * expr (** assignment *)
  | ProcCall of expr (** procedure calling *)
  | If of expr * statement list * statement list (** if statement *)
  | While of expr * statement list (** while loop *)
  | Repeat of expr * statement list (** repeat-until loop *)
  | For of name * expr * expr * statement list (** for loop with counter *)
  | Break (** break *)
  | Continue (** continue *)
[@@deriving show]

(** definition *)
and define =
  | DType of name * vtype (** type definition *)
  | DNDVariable of name * vtype (** variable definition without assignment *)
  | DVariable of name * vtype * expr (** variable definition with assignment by expr *)
  | DDVariable of name * vtype * value (** variable definition with assignment by value *)
  | DConst of name * expr (** const definition by expr *)
  | DDConst of name * value (** const definition by value *)
  | DFunction of name * vtype * fun_param list * t (** function definition *)
[@@deriving show]

(** world item *)
and variable =
  | VConst of value (** constant *)
  | VVariable of value (** variable *)
  | VType (** type *)
[@@deriving show]

(** world, which contains all info about variables *)
and world = (vtype * variable) KeyMap.t [@@deriving show]

(** Pascal program of definition list and statement list *)
and t = define list * statement list [@@deriving show]
