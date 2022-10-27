(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

val eval_binop : binop -> value -> value -> value

val eval_binop_type : binop -> vtype -> vtype -> vtype

val eval_unop : unop -> value -> value

val eval_unop_type : unop -> vtype -> vtype

val eval_std_function : name -> value list -> value

val eval_std_function_type : name -> vtype list -> vtype

val get_rec : value -> name -> value

val get_rec_type : vtype -> name -> vtype

val iter_arr : value -> value -> int

val get_arr : value -> value -> value

val get_arr_type : vtype -> vtype -> vtype

val eval_expr_base :
  (world -> name -> value) ->
  (world -> value -> expr list -> value) ->
  world ->
  expr ->
  value
(** loader -> eval_function -> world -> expr -> value*)

val eval_expr_base_type :
  (world -> name -> vtype) ->
  (world -> vtype -> expr list -> vtype) ->
  world ->
  expr ->
  vtype
(** type_loader -> eval_function_type -> world -> expr -> type*)

val eval_expr_const : world -> expr -> value
