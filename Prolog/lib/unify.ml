(** Copyright 2021-2022, Ilya Shchuckin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Contains Herbrand algorithm (7.3.2 in ISO)*)

open Ast
open Types
open Utils

module Unify (M : MonadFail) = struct
  open M

  (**/**)

  let eval_error x = fail (EvalError x)

  (**/**)

  (** [is_var_in_term term var] check if [var] is in a [term]. *)
  let rec is_var_in_term term var =
    match term with
    | Atomic _ -> false
    | Var _ when equal_term term var -> true
    | Compound { atom = _; terms } ->
      List.fold_left (fun acc term -> acc || is_var_in_term term var) false terms
    | _ -> false
  ;;

  (** Herbrand algorithm (7.3.2. in ISO). 
      Takes a list of pairs [(term1, term2)] (equations: term1 = term2) and tries to find 
      the most general unifier of this list. Returns unifier if successful.  *)
  let rec unify equations : unifier result =
    let fail (term1, term2) =
      eval_error
        (String.concat
           ""
           [ "Not unifiable: \n"; show_term term1; " with: \n"; show_term term2; "\n" ])
    in
    match equations with
    | ((term1, term2) as pair) :: tl ->
      (match term1, term2 with
       | Atomic x, Atomic y when not (equal_atomic x y) -> fail pair (* a.1 *)
       | Atomic _, Compound _ -> fail pair (* a.2 *)
       | Compound { atom = atom1; terms = _ }, Compound { atom = atom2; terms = _ }
         when not (equal_atom atom1 atom2) -> fail pair (* a.3 *)
       | Compound { atom = _; terms = t1 }, Compound { atom = _; terms = t2 }
         when not (List.length t1 = List.length t2) -> fail pair (* a.4 *)
       | Var x, Var y when Base.equal_string x y -> unify tl (* b *)
       | Atomic x, Atomic y when equal_atomic x y -> unify tl (* c *)
       | ( Compound { atom = atom1; terms = terms1 }
         , Compound { atom = atom2; terms = terms2 } )
         when equal_atom atom1 atom2 (* d *) ->
         let rec unify_lists (list1, list2) =
           match list1, list2 with
           | h1 :: tl1, h2 :: tl2 ->
             unify [ h1, h2 ]
             >>= fun res ->
             unify_lists
               ( List.map (fun x -> apply_substitution x res) tl1
               , List.map (fun x -> apply_substitution x res) tl2 )
             >>= fun res_list -> return (res @ res_list)
           | _ -> return []
         in
         unify_lists (terms1, terms2)
       | (Atomic _ as x), (Var _ as y) -> unify ((y, x) :: tl) (* e *)
       | (Compound _ as x), (Var _ as y) -> unify ((y, x) :: tl) (* e *)
       | (Var _ as y), term when not (is_var_in_term term y) (* f *) ->
         unify
           (List.map
              (fun (h, b) ->
                apply_substitution h [ y, term ], apply_substitution b [ y, term ])
              tl)
         >>= fun res -> return ((y, term) :: res)
       | (Var _ as y), (Compound _ as term) when is_var_in_term term y (* g *) ->
         fail (term, y)
       | pair -> fail pair)
    | _ -> return []
  ;;
end
