(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

val eval_binop : binop -> value -> value -> value
val eval_binop_type : binop -> vtype -> vtype -> vtype
val eval_unop : unop -> value -> value
val eval_unop_type : unop -> vtype -> vtype
val eval_std_function : name -> value list -> value
val eval_std_function_type : name -> vtype list -> vtype

(** field -> record -> value *)
val get_rec : name -> value -> value

(** field -> record -> vtype *)
val get_rec_type : name -> vtype -> vtype

val iter : value -> int -> value

(** arr start -> ind value -> real ind*)
val iter_arr : value -> value -> int

val is_iterable : vtype -> bool

(** index -> array -> value *)
val get_arr : value -> value -> value

(** index -> array -> vtype *)
val get_arr_type : vtype -> vtype -> vtype

(** loader -> eval_function -> expr -> Worlds.t -> (value, Worlds.t) *)
val eval_expr_base
  :  (name -> Worlds.t -> value)
  -> (expr -> expr list -> Worlds.t -> value * Worlds.t)
  -> expr
  -> Worlds.t
  -> value * Worlds.t

(** loader -> eval_function -> expr -> Worlds.t -> (vtype, Worlds.t) *)
val eval_expr_base_type
  :  (name -> Worlds.t -> vtype)
  -> (expr -> expr list -> Worlds.t -> vtype * Worlds.t)
  -> expr
  -> Worlds.t
  -> vtype * Worlds.t

(** eval constant expression, can be used only for constants and std functions *)
val eval_expr_const : expr -> Worlds.t -> value
