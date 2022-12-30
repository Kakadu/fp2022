(** Copyright 2021-2022, Ilya Shchuckin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

(** [get_pi term] returns predicate indicator (7.1.6.6 in ISO) of [term]. *)
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

(** [str_of_pi pi] converts predicate indicator [pi] to string. *)
let str_of_pi = function
  | str, n -> String.concat "/" [ str; string_of_int n ]
;;

(** [get_vars_from_term term] returns a list of variables that occur in [term]. *)
let rec get_vars_from_term = function
  | Atomic _ -> []
  | Var str -> [ Var str ]
  | Compound { atom = _; terms } ->
    List.fold_left (fun acc term -> acc @ get_vars_from_term term) [] terms
;;

(** [apply_substitution term substitution] replaces variables in [term] 
    with matching substitions from [substitution]. *)
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

(** Check whether a list is empty.  *)
let is_empty list = list = []
