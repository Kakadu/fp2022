(** Copyright 2021-2022, Ilya Shchuckin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Defines database (7.5 in ISO) *)

open Ast
open Types
open Utils
open Unify

module Db (M : MonadFail) = struct
  open M
  open Unify (M)

  (**/**)

  let critical x = fail (Critical x)
  let eval_error x = fail (EvalError x)

  (**/**)

  (** [term_to_clause term] converts term to a clause *)
  let term_to_clause term =
    let fail term =
      critical
        (String.concat "" [ "Can't convert term "; Ast.show_term term; "to clause" ])
    in
    match term with
    | Compound { atom; terms } when Ast.equal_atom atom (Operator ":-") ->
      (match terms with
       | [ head; goal ] -> return { head; goal }
       | _ -> fail term)
    | Compound _ -> return { head = term; goal = Atomic (Atom (Name "true")) }
    | Atomic _ -> return { head = term; goal = Atomic (Atom (Name "true")) }
    | _ -> fail term
  ;;

  (** [terms_to_clauses terms] converts list of terms to a list of clauses *)
  let rec terms_to_clauses = function
    | hd :: tl -> lift2 (fun r1 r2 -> r1 :: r2) (term_to_clause hd) (terms_to_clauses tl)
    | _ -> return []
  ;;

  (** [rename_clause_vars head body] gives new names to variables in a clause. 
      Helps to avoid conflicts in substitutions. *)
  let rename_clause_vars head body =
    let vars = get_vars_from_term head @ get_vars_from_term body in
    List.fold_left
      (fun acc -> function
        | Var _ -> acc
        | _ -> critical "Var list cannot have non-var terms in it")
      (Ok "")
      vars
    *> return
         (List.map
            (function
             | Var s ->
               ( Var s
               , Var
                   (String.concat
                      ""
                      [ s; "__"; string_of_float (Sys.time () *. 1000000.) ]) )
             | _ -> Var "_", Var "_")
            vars)
    >>| fun substitution ->
    apply_substitution head substitution, apply_substitution body substitution
  ;;

  (** [prepare text] converts prolog text to the database. *)
  let prepare (program_text : string) : db result =
    let terms = Parser.parse_program program_text in
    match terms with
    | Error _ -> critical "Failed to parse the program"
    | Ok db -> terms_to_clauses db
  ;;

  (** [search db goal] searches in db for a term that can be unified with goal. *)
  let rec search (db : db) goal =
    let goal_pi = get_pi goal in
    match db with
    | { head; goal = clause_goal } :: tl ->
      rename_clause_vars head clause_goal
      >>= fun (head, clause_goal) ->
      if get_pi head = goal_pi
      then (
        let unifiable =
          match unify [ head, goal ] with
          | Ok _ -> true
          | Error _ -> false
        in
        match search tl goal with
        | Ok x ->
          if unifiable then return ({ head; goal = clause_goal } :: x) else return x
        | Error _ ->
          if unifiable then return [ { head; goal = clause_goal } ] else return [])
      else search tl goal
    | _ -> eval_error ("Error: No such predicate in DB for PI " ^ str_of_pi goal_pi)
  ;;
end
