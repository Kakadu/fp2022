(** Copyright 2021-2022, Danila Pechenev & Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base

(** Finds all identifiers in an expression. Used in pattern-matching inference *)
let rec find_identifiers = function
  | EBinaryOperation (_, left_operand, right_operand) ->
    find_identifiers left_operand @ find_identifiers right_operand
  | EUnaryOperation (_, operand) -> find_identifiers operand
  | EApplication (left_operand, right_operand) ->
    find_identifiers left_operand @ find_identifiers right_operand
  | EIdentifier id -> [ id ]
  | EFun _ -> []
  | EList expression_list | ETuple expression_list ->
    List.fold_right
      ~f:(fun expression acc -> find_identifiers expression @ acc)
      ~init:[]
      expression_list
  | EConstructList (operand, list) -> find_identifiers operand @ find_identifiers list
  | EDataConstructor (_, expression) ->
    (match expression with
     | Some expression -> find_identifiers expression
     | _ -> [])
  | EEffectPattern expression -> find_identifiers expression
  | EEffectArg (_, expression) -> find_identifiers expression
  | _ -> []
;;
