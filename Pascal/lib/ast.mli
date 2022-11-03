(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string

val pp_name : Ppx_show_runtime.Format.formatter -> name -> unit
val show_name : name -> string

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

val pp_binop : Ppx_show_runtime.Format.formatter -> binop -> unit
val show_binop : binop -> string

(** unar operators *)
type unop =
  | Plus (** ( + ) *)
  | Minus (** ( - ) *)
  | Not (** ( not ) *)

val pp_unop : Ppx_show_runtime.Format.formatter -> unop -> unit
val show_unop : unop -> string

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
  | VTVoid (** special type for procedure declaration *)

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
  | VVoid
      (** special type for procedure declaration or for an unassigned function variable *)

(** function parameters *)
and fun_param =
  | FPFree of name * vtype (** standard parameter *)
  | FPOut of name * vtype (** out parameter *)
  | FPConst of name * vtype (** const parameter *)

(** expression *)
and expr =
  | Const of value (** const *)
  | Variable of name (** variable *)
  | BinOp of binop * expr * expr (** binary operator *)
  | UnOp of unop * expr (** unary operator *)
  | Call of expr * expr list (** function calling *)
  | GetRec of expr * name (** record field getting *)
  | GetArr of expr * expr (** array field getting *)

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
  | Exit (** exit *)

(** definition *)
and define =
  | DType of name * vtype (** type definition *)
  | DNDVariable of name * vtype (** variable definition without assignment *)
  | DVariable of name * vtype * expr (** variable definition with assignment by expr *)
  | DDVariable of name * vtype * value (** variable definition with assignment by value *)
  | DConst of name * expr (** const definition by expr *)
  | DDConst of name * value (** const definition by value *)
  | DFunction of name * vtype * fun_param list * t (** function definition *)

(** world item *)
and variable =
  | VConst of value (** constant *)
  | VVariable of value (** variable *)
  | VFunctionResult of value (** variable for function result *)
  | VType (** type *)

(** world, which contains all info about variables *)
and world = (vtype * variable) KeyMap.t

(** Pascal program of definition list and statement list *)
and t = define list * statement list

val pp_vtype : Ppx_show_runtime.Format.formatter -> vtype -> unit
val show_vtype : vtype -> string
val pp_value : Ppx_show_runtime.Format.formatter -> value -> unit
val show_value : value -> string
val pp_fun_param : Ppx_show_runtime.Format.formatter -> fun_param -> unit
val show_fun_param : fun_param -> string
val pp_expr : Ppx_show_runtime.Format.formatter -> expr -> unit
val show_expr : expr -> string
val pp_statement : Ppx_show_runtime.Format.formatter -> statement -> unit
val show_statement : statement -> string
val pp_define : Ppx_show_runtime.Format.formatter -> define -> unit
val show_define : define -> string
val pp_variable : Ppx_show_runtime.Format.formatter -> variable -> unit
val show_variable : variable -> string
val pp_world : Ppx_show_runtime.Format.formatter -> world -> unit
val show_world : world -> string
val pp : Ppx_show_runtime.Format.formatter -> t -> unit
val show : t -> string
