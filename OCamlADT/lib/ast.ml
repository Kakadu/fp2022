(** Copyright 2021-2022, Kakadu, EmirVildanov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Abstract Syntax Tree module *)

(** Alias for built-in OCaml string type. Represents identifiers *)
type id = string [@@deriving show { with_path = false }]

(** Mapping of declared variables and function
    into their values *)
module IdMap = Map.Make (struct
  type t = id

  let compare = compare
end)

(** Constants type: values that can't be altered during program execution *)
type constant =
  | Bool of bool (** true/false *)
  | Int of int (** Integer literals *)
  | Str of string (** String literals *)
  | Nil (** Empty list [] *)
  | Unit (** () value *)
[@@deriving show { with_path = false }]

(** Binary operations type *)
type binary_op =
  | Plus (** + *)
  | Minus (** - *)
  | Mult (** * *)
  | Divide (** / *)
  | Eq (** = *)
[@@deriving show { with_path = false }]

(* Unary operations type *)
type unary_op = UnaryMinus (** - *) [@@deriving show { with_path = false }]

(** General expressions type *)
type expr =
  | Constant of constant (** Wrapper for constants *)
  | Tuple of expr list (** Tuple like (a, b, d) *)
  | BinaryOp of binary_op * expr * expr
      (** Binary operator like [left_expr] [op] [right_expr] *)
  | UnaryOp of unary_op * expr (** Unary operator like -3 *)
  | Var of id (** Wrapper for variables *)
  | Fun of id * expr
      (** Representation of ananymous
    * function of view "fun x -> e"
    * NOTE: "fun x y -> e" is desugared
    * into Fun(Var("x"), Fun(Var("y"), e)) *)
  | Cons of expr * expr (** List construction like "h::t" *)
  | IfThenElse of expr * expr * expr
      (** Condition expression like "if a then b else c"
        * NOTE: Calculated in a lazy manner *)
  | Let of bool * id * Typing.t option * expr
      (** Declaration like "let x: [type] = expr_1"
        * boolean flag showing if it's `rec`
        * NOTE: "let f x = e" is desugared
        * into Let(Var("f"), Fun("x", e)) *)
  | LetIn of expr * expr
      (** LetIn (Let (...), expr)                          
        * They are splitted in order to exlude
        * opportunity of appearing `let` without `in`
        * in nested expresstion *)
  | App of expr * expr
      (** Function application like "f e"
        * NOTE: Implemented in a call_by_value manner
        * (e must be calculated before substitution).
        * As an applicator there may be a function
        * variable or lambda or smth else. That's why App
        * is (expr * expr) and not (id * expr) *)
  | Match of expr * (expr * expr) list
      (** Match like
        * "match e with
        * | p1 -> e1
        * ...
        * | pn -> en" *)
  | ADT of id * expr list (** Constuctor of Algebraic Data Types *)
  | Type of id * Typing.t (** Type declaration *)
[@@deriving show { with_path = false }]

let nil_adt_name = "Nil"
let cons_adt_name = "Cons"

(** Intermediate result of interpreter evaluation process *)
type value =
  | VUndef
  | VNil
  | VUnit
  | VBool of bool
  | VInt of int
  | VString of string
  | VCons of value * value
  | VClosure of
      value IdMap.t
      * id
      * expr (* Result of Fun evaluation, where id is a Fun paramenter *)
  | VRecClosure of id * value IdMap.t * id * expr
(* Result of RecFun evaluation, where first 
     id is a Fun name and second id is a Fun parameter *)

type environment = value IdMap.t

type evaluation_result =
  { value : value
  ; env : environment
  }
