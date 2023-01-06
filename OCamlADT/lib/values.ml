(** Copyright 2021-2022, Kakadu, EmirVildanov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Module that represents values appeared during code evaluation.
    Including results of intermidiate calculation *)

open Ast

(** Mapping of declared variables and function
    into their values *)
module IdMap = Map.Make (struct
  type t = id

  let compare = compare
end)

(** Intermediate result of interpreter evaluation process *)
type value =
  | VUndef
  | VNil
  | VUnit
  | VBool of bool
  | VInt of int
  | VString of string
  | VCons of value * value
  | VClosure of environment * id * expr

and environment = value ref IdMap.t

(* Pair of expression evaluation result and updated environmet.
   Environment is returned since we have to share environemt
   through several REPL interpreter calls. *)
type evaluation_result =
  { value : value
  ; env : environment
  }
