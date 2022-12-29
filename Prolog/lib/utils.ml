(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

let get_pi =
  let atom_indicator = function
    | Name str -> str
    | Operator str -> str
  in
  let atomic_indicator = function
    | Num n -> string_of_int n
    | Atom x -> atom_indicator x
  in
  function
  | Atomic x -> atomic_indicator x, 0
  | Var str -> str, 0
  | Compound { atom; terms } -> atom_indicator atom, List.length terms
;;

let str_of_pi pi =
  match pi with
  | str, n -> str ^ "/" ^ string_of_int n
;;

let rec get_vars_from_term = function
  | Atomic _ -> []
  | Var str -> [ Var str ]
  | Compound { atom = _; terms } ->
    List.fold_left (fun acc term -> acc @ get_vars_from_term term) [] terms
;;

let rec apply_substitution term substitution =
  match term with
  | Var _ ->
    (match substitution with
     | (head, body) :: _ when equal_term head term -> body
     | _ :: tl -> apply_substitution term tl
     | _ -> term)
  | Atomic _ -> term
  | Compound { atom; terms } ->
    let new_terms = List.map (fun term -> apply_substitution term substitution) terms in
    Compound { atom; terms = new_terms }
;;

let is_empty list = List.length list == 0
