(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Builtinops
open Environment
open Utils

let rec eval (st : State.storage) (code : ast) : value * State.storage =
  let rec eval_function
    (f_name : string)
    (p_names : string list)
    (body : ast)
    (p_values : value list)
    : value
    =
    let () =
      if not (List.length p_names = List.length p_values)
      then failwith "Wrong number of arguments."
    in
    let state = State.add_state ~global:st ~local:State.create in
    let state =
      State.set_variable
        state
        f_name
        (Function (f_name, p_names, eval_function f_name p_names body))
    in
    let params = List.combine p_names p_values in
    let step st (n, v) = State.set_variable st n v in
    let initiated = List.fold_left step state params in
    fst (eval initiated body)
  in
  match code with
  | Literal (lit_t, v) -> value_of_literal lit_t v, st
  | Var n -> State.get_variable st n, st
  | VarAssign (i, v) ->
    let var_value, st = eval st v in
    let new_state = State.set_variable st i var_value in
    var_value, new_state
  | Binop (op, l, r) ->
    let op_f = match_binop op in
    let l_v, st = eval st l in
    let r_v, st = eval st r in
    op_f l_v r_v, st
  | Conditional (cond, thenB, elseB) ->
    let cond_v, st = eval st cond in
    eval st (conditional cond_v thenB elseB)
  | Seq lst ->
    List.fold_left (fun (_, betw_exp_st) el -> eval betw_exp_st el) (Nil, st) lst
  | WhileLoop (cond, body) ->
    let rec iteration s =
      let c_v, n_st = eval s cond in
      match c_v with
      | Bool v when v ->
        let _, n_st = eval n_st body in
        iteration n_st
      | Bool v when not v -> n_st
      | _ -> typefail "While loop expected bool as condition"
    in
    Nil, iteration st
  | ArrayDecl lst ->
    let values, new_st =
      List.fold_left
        (fun (acc, betw_exp_st) el ->
          match eval betw_exp_st el with
          | s_v, s_s -> s_v :: acc, s_s)
        ([], st)
        lst
    in
    Array (List.rev values), new_st
  | Indexing (box, ind) ->
    let b_v, n_st = eval st box in
    let i_v, n_st = eval n_st ind in
    indexing b_v i_v, n_st
  | FuncDeclaration (name, params, body) ->
    ( Nil
    , State.set_variable st name (Function (name, params, eval_function name params body))
    )
  | Invocation (box_inv, params) ->
    let left, n_st = eval st box_inv in
    let params, n_st =
      List.fold_left
        (fun (acc, betw_exp_st) el ->
          match eval betw_exp_st el with
          | s_v, s_s -> s_v :: acc, s_s)
        ([], n_st)
        params
    in
    let params = List.rev params in
    (match left with
     | Function (_, _, f) -> f params, n_st
     | _ -> typefail "")
;;

let run (code : ast) = fst (eval Stdlib.initial_state code)
