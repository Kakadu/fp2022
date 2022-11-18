(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string [@@deriving show { with_path = false }]

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
[@@deriving show { with_path = false }]

(** unary operators *)
type unop =
  | Plus (** ( + ) *)
  | Minus (** ( - ) *)
  | Not (** ( not ) *)
[@@deriving show { with_path = false }]

(** types which parser returns *)
type ptype =
  | PTBool (** boolean type *)
  | PTInt (** integer type *)
  | PTFloat (** real type *)
  | PTChar (** char type *)
  | PTVoid (** special type for procedure declaration *)
  | PTString (** string type without len, will be used 255 *)
  | PTDString of expr (** string type with expr len *)
  | PTRecord of (name * ptype) list (** record type of definitions *)
  | PTFunction of ptype fun_param list * ptype (** function type *)
  | PTArray of expr * expr * ptype (** array type with expr..expr interval *)
  | PTCustom of name (** custom type *)
[@@deriving show { with_path = false }]

(** virtual types *)
and vtype =
  | VTBool (** boolean type *)
  | VTInt (** integer type *)
  | VTFloat (** real type *)
  | VTChar (** char type *)
  | VTVoid (** special type for procedure declaration *)
  | VTString of int (** string type with len *)
  | VTRecord of vtype KeyMap.t (** record type constructed *)
  | VTFunction of vtype fun_param list * vtype (** function type *)
  | VTConstFunction of vtype fun_param list * vtype (** function actual body type *)
  | VTArray of value * int * vtype
      (** array type with calculated interval : (from <val> with <size> of <type>) *)
[@@deriving show { with_path = false }]

(** virtual values *)
and value =
  | VBool of bool (** boolean value *)
  | VInt of int (** integer value *)
  | VFloat of float (** real value *)
  | VChar of char (** char value *)
  | VString of string * int (** string value *)
  | VRecord of world (** record value *)
  | VFunction of name * vtype * vtype fun_param list * world * statement list
      (** function value : (<name> <result type> <param list> <local world> <realization>) *)
  | VArray of value * int * vtype * value ImArray.t
      (** array value : from <val> with <size> of <type>, contains <arr>*)
  | VVoid
      (** special type for procedure declaration or for an unassigned function variable *)
[@@deriving show { with_path = false }]

(** function parameters *)
and 'a fun_param =
  | FPFree of name * 'a (** standard parameter *)
  | FPOut of name * 'a (** out parameter *)
  | FPConst of name * 'a (** const parameter *)
[@@deriving show { with_path = false }]

(** expression *)
and expr =
  | Const of value (** const *)
  | Variable of name (** variable *)
  | BinOp of binop * expr * expr (** binary operator *)
  | UnOp of unop * expr (** unary operator *)
  | Call of expr * expr list (** function calling *)
  | GetRec of expr * name (** record field getting *)
  | GetArr of expr * expr (** array field getting *)
[@@deriving show { with_path = false }]

(** statement *)
and statement =
  | Assign of expr * expr (** assignment *)
  | AssignFunc of expr * name (** assignment of function *)
  | ProcCall of expr * expr list (** procedure calling *)
  | If of expr * statement list * statement list (** if statement *)
  | While of expr * statement list (** while loop *)
  | Repeat of expr * statement list (** repeat-until loop *)
  | For of name * expr * expr * statement list (** for loop with counter *)
  | Break (** break *)
  | Continue (** continue *)
  | Exit (** exit *)
[@@deriving show { with_path = false }]

(** definition *)
and define =
  | DType of name * ptype (** type definition *)
  | DVariable of name * ptype (** variable definition without assignment *)
  | DDVariable of name * ptype * expr (** variable definition with assignment by expr *)
  | DConst of name * expr (** const definition by expr *)
  | DDConst of name * value (** const definition by value *)
  | DFunction of name * ptype * ptype fun_param list * t (** function definition *)
[@@deriving show { with_path = false }]

(** world item *)
and variable =
  | VConst of value (** constant *)
  | VVariable of value (** variable *)
  | VFunctionResult of value (** variable for function result *)
  | VType (** type *)
[@@deriving show { with_path = false }]

(** world, which contains all info about variables *)
and world = (vtype * variable) KeyMap.t [@@deriving show { with_path = false }]

(** Pascal program of definition list and statement list *)
and t = define list * statement list [@@deriving show { with_path = false }]
