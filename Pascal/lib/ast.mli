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

(** unary operators *)
type unop =
  | Plus (** ( + ) *)
  | Minus (** ( - ) *)
  | Not (** ( not ) *)

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

(** virtual types *)
and vtype =
  | VTBool (** boolean type *)
  | VTInt (** integer type *)
  | VTFloat (** real type *)
  | VTChar (** char type *)
  | VTVoid (** special type for procedure declaration *)
  | VTString of int (** string type with len *)
  | VTRecord of vtype KeyMap.t (** record type constructed *)
  | VTFunction of vtype fun_param list * vtype (** function variable type *)
  | VTConstFunction of vtype fun_param list * vtype (** function actual body type *)
  | VTArray of value * int * vtype
      (** array type with calculated interval : (from <val> with <size> of <type>) *)

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

(** function parameters *)
and 'a fun_param =
  | FPFree of name * 'a (** standard parameter *)
  | FPOut of name * 'a (** out parameter *)
  | FPConst of name * 'a (** const parameter *)

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
  | AssignFunc of expr * name (** assignment of function *)
  | ProcCall of expr * expr list (** procedure calling *)
  | If of expr * statement list * statement list (** if statement *)
  | While of expr * statement list (** while loop *)
  | Repeat of expr * statement list (** repeat-until loop *)
  | For of name * expr * expr * statement list (** for loop with counter *)
  | Break (** break *)
  | Continue (** continue *)
  | Exit (** exit *)

(** definition *)
and define =
  | DType of name * ptype (** type definition *)
  | DVariable of name * ptype (** variable definition without assignment *)
  | DDVariable of name * ptype * expr (** variable definition with assignment by expr *)
  | DConst of name * expr (** const definition by expr *)
  | DDConst of name * value (** const definition by value *)
  | DFunction of name * ptype * ptype fun_param list * t (** function definition *)

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

val pp_name : Ppx_show_runtime.Format.formatter -> name -> unit
val show_name : name -> string
val pp_binop : Ppx_show_runtime.Format.formatter -> binop -> unit
val show_binop : binop -> string
val pp_unop : Ppx_show_runtime.Format.formatter -> unop -> unit
val show_unop : unop -> string
val pp_ptype : Ppx_show_runtime.Format.formatter -> ptype -> unit
val show_ptype : ptype -> string
val pp_vtype : Ppx_show_runtime.Format.formatter -> vtype -> unit
val show_vtype : vtype -> string
val pp_value : Ppx_show_runtime.Format.formatter -> value -> unit
val show_value : value -> string

val pp_fun_param
  :  (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a fun_param
  -> unit

val show_fun_param : (Format.formatter -> 'a -> unit) -> 'a fun_param -> name
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
