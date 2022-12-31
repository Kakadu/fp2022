(** Copyright 2021-2022, Ilya Shchuckin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Monadic interpreter *)

open Ast
open Utils
open Unify
open Types
open Db
open Print

module Interpret (M : MonadFail) (C : Config) = struct
  open M
  open C
  open Unify (M)
  open Db (M)

  (**/**)

  let critical x = fail (Critical x)
  let eval_error x = fail (EvalError x)

  (**/**)

  type builtin_predicate =
    | True
    | Cut
    | Read of term
    | Writeln of term
    | Identical of term * term
    | Clause_p of term * term
    | Conjuction_p of term * term

  (** {2 Helpers} *)

  (** [is_builtin goal] returns builtin predicate if [goal] matches with builtin predicate*)
  let is_builtin (term : term) =
    match term with
    | Atomic (Atom (Name "true")) -> `Builtin True
    | Atomic (Atom (Name "!")) -> `Builtin Cut
    | Compound { atom = Name "read"; terms = [ term ] } -> `Builtin (Read term)
    | Compound { atom = Name "writeln"; terms = [ term ] } -> `Builtin (Writeln term)
    | Compound { atom = Name "clause"; terms = [ term1; term2 ] } ->
      `Builtin (Clause_p (term1, term2))
    | Compound { atom = Operator ","; terms = [ term1; term2 ] } ->
      `Builtin (Conjuction_p (term1, term2))
    | Compound { atom = Operator "=="; terms = [ term1; term2 ] } ->
      `Builtin (Identical (term1, term2))
    | _ -> `NotBuiltin
  ;;

  (** [swap_vars sub] if pair [(Var x, Var y)] is in [sub], it will be replaced with [(Var y, Var x)]. 
      This is helpfull when reconstructing the result. *)
  let swap_vars substitution =
    List.map
      (function
       | Var x, Var y -> Var y, Var x
       | x -> x)
      substitution
  ;;

  (** [filter_substitution sub vars] leaves in [sub] only those pairs, 
      that contain variable from [vars] as the first element, e.g. [(Var x, something)], where [Var x] is in [vars]. *)
  let rec filter_substitution sub (vars : term list) =
    match vars with
    | var :: tl ->
      (match
         try Ok (List.find (fun (head, _) -> equal_term head var) sub) with
         | Not_found -> eval_error "no such variable in the unifier"
       with
       | Ok elem -> elem :: filter_substitution sub tl
       | Error _ -> filter_substitution sub tl)
    | _ -> []
  ;;

  (** {2 Evaluators} *)

  (** Evaluates second branch in a conjuction. 
      If it fails, it backtracks first branch choicepoints. *)
  let rec eval_right_branch r_branch substitution l_subs choicepoints l_choicepoints =
    let right_branch_result = eval (apply_substitution r_branch l_subs) l_subs [] in
    right_branch_result
    >>= (fun (r_subs, r_choicepoints) ->
          let conjuction =
            if is_empty l_choicepoints
            then []
            else
              [ Conjuction
                  { goal = r_branch; choicepoints = l_choicepoints; substitution }
              ]
          in
          unify (r_subs @ l_subs)
          >>= fun res -> return (res, r_choicepoints @ conjuction @ choicepoints))
    <|> fun () ->
    backtrack l_choicepoints
    >>= fun (l_subs, r_choicepoints) ->
    eval_right_branch r_branch substitution l_subs choicepoints r_choicepoints

  (** [eval_builtin goal substitution choicepoints] evaluates [goal] which is a 
      variant of builtin predicate. *)
  and eval_builtin goal substitution choicepoints =
    (* let goal_pi = get_pi goal in *)
    let is_cut term = get_pi term = ("!", 0) in
    match goal with
    | True | Cut -> return (substitution, choicepoints)
    | Conjuction_p (left_branch, right_branch) when is_cut left_branch ->
      eval right_branch substitution []
      >>= fun (unifier, _) -> return (unifier, choicepoints)
    | Conjuction_p (left_branch, right_branch) when is_cut right_branch ->
      eval left_branch substitution []
      >>= fun (unifier, _) -> return (unifier, choicepoints)
    | Conjuction_p (left_branch, right_branch) ->
      eval left_branch substitution []
      >>= fun (l_subs, l_choicepoints) ->
      eval_right_branch right_branch substitution l_subs choicepoints l_choicepoints
    | Identical (term1, term2) ->
      if equal_term term1 term2
      then return (substitution, choicepoints)
      else eval_error "terms are not equal"
    | Read term ->
      (match In_channel.input_line In_channel.stdin with
       | Some query_text -> return query_text
       | None -> critical "Unexpected EOF")
      >>= fun query_text ->
      (match Parser.parse_query query_text with
       | Error _ -> critical "can't parse the input string"
       | Ok x ->
         unify [ x, term ]
         >>= fun res ->
         unify (res @ substitution) >>= fun res -> return (res, choicepoints))
    | Writeln term ->
      (match str_of_term term with
       | Ok str ->
         Caml.Format.printf "%s\n" str;
         return (substitution, choicepoints)
       | Error _ -> critical "Cannot print term")
    | Clause_p (head, body) ->
      (match is_builtin head with
       | `Builtin _ -> eval_error "Can't unify clause with builtins"
       | `NotBuiltin ->
         search db head
         >>= fun candidates ->
         let unifiable =
           List.fold_left
             (fun (acc : bool) { head = head2; goal = body2 } ->
               match
                 unify [ head, head2 ]
                 >>= fun unificator -> unify ([ body, body2 ] @ unificator)
               with
               | Ok _ -> true
               | Error _ -> false || acc)
             false
             candidates
         in
         if not unifiable
         then eval_error "Can't unify clauses"
         else (
           let choicepoints =
             Clause { substitution; candidates; head; body } :: choicepoints
           in
           backtrack choicepoints))

  (** {2 Backtracking} *)
  (** There are three types of choicepoints in the interpreter. 
      - [Conjuction] - describes how to redo [(,)/2] predicate
      - [Clause] - describes how to redo [clause/2] predicate
      - [Choicepoint] - describes how to redo a goal evaluation 
      via picking another unifiable from the database.
  *)

  and backtrack_clause_builtin clause choicepoints =
    match clause with
    | Clause { substitution; candidates; head = head1; body = body1 } ->
      (match candidates with
       | { head = head2; goal = body2 } :: tl ->
         unify [ head2, head1 ]
         >>= fun head_unificator ->
         unify [ body2, body1 ]
         >>= fun body_unificator ->
         unify (head_unificator @ body_unificator @ substitution)
         >>| fun sub ->
         ( sub
         , if is_empty tl
           then choicepoints
           else
             Clause { substitution; candidates = tl; head = head1; body = body1 }
             :: choicepoints )
       | _ -> eval_error "no more candidates")
    | _ -> critical "wrong choicepoint type was passed to backtrack_clause_builtin"

  and backtrack_conjuction conjuction choicepoints =
    match conjuction with
    | Conjuction { goal; choicepoints = c_choicepoints; substitution } ->
      backtrack c_choicepoints
      >>= (fun (subs, new_choicepoints) ->
            eval_right_branch goal substitution subs choicepoints new_choicepoints)
      <|> fun () -> backtrack choicepoints
    | _ -> critical "wrong choicepoint type was passed to backtrack_conjuction"

  and backtrack_choicepoint choicepoint choicepoints =
    let debug_print goal =
      if debug
      then (
        match str_of_term goal with
        | Ok str -> Caml.Format.printf "Failed: %s\nBacktracking \n\n" str
        | Error _ ->
          Caml.Format.printf "Error: cannot properly print term %s \n" (show_term goal))
    in
    match choicepoint with
    | Choicepoint { goal; substitution; candidates } ->
      let try_backtrack head clause_goal choicepoints =
        unify [ head, goal ]
        >>= fun unificator ->
        let new_body = apply_substitution clause_goal unificator in
        unify (unificator @ substitution)
        >>= fun new_sub -> eval new_body new_sub choicepoints
      in
      (match candidates with
       | [ { head; goal = clause_goal } ] ->
         (match try_backtrack head clause_goal choicepoints with
          | Ok x -> return x
          | Error x ->
            debug_print goal;
            Error x)
       | { head; goal = clause_goal } :: tl ->
         let choicepoints =
           Choicepoint { goal; substitution; candidates = tl } :: choicepoints
         in
         try_backtrack head clause_goal choicepoints
         <|> fun () ->
         debug_print goal;
         backtrack choicepoints
       | _ ->
         debug_print goal;
         eval_error "No more candidates left for predicat")
    | _ -> critical "wrong choicepoint type was passed to backtrack_choicepoint"

  (** [backtrack choicepoints] starts backtracking from 
      the most recent choicepoint in [choicepoints].*)
  and backtrack = function
    | (Clause _ as x) :: choicepoints_tl -> backtrack_clause_builtin x choicepoints_tl
    | (Conjuction _ as x) :: choicepoints_tl -> backtrack_conjuction x choicepoints_tl
    | (Choicepoint _ as x) :: choicepoints_tl -> backtrack_choicepoint x choicepoints_tl
    | _ -> eval_error "No more choicepoints"

  (** {2 Run} *)

  (** [eval goal substitution choicepoints] tries to satisfy the [goal]. 
      If successful returns updated substituion and choicepoints.*)
  and eval goal substitution choicepoints =
    if debug
    then (
      (match str_of_term goal with
       | Ok str -> Caml.Format.printf "Goal: %s\n" str
       | Error _ ->
         Caml.Format.printf "Error: cannot properly print term %s \n" (show_term goal));
      Print.print_substitution substitution;
      Caml.Format.printf "\n\n");
    match is_builtin goal with
    | `Builtin goal -> eval_builtin goal substitution choicepoints
    | `NotBuiltin ->
      search db goal
      >>= fun candidates ->
      let choicepoints = Choicepoint { goal; substitution; candidates } :: choicepoints in
      backtrack choicepoints
  ;;

  (** This function is the one that can be returned as the result of [run] function. 
      It allows to start backtracking if user needs more suitable sets of variables. *)
  let rec run_backtrack filter_sub choicepoints =
    match backtrack choicepoints with
    | Ok (subs, choicepoints) ->
      let search_tree_depleted = is_empty choicepoints in
      if not search_tree_depleted
      then
        return
          (InterpretationResult
             (filter_sub subs, Some (fun () -> run_backtrack filter_sub choicepoints)))
      else return (InterpretationResult (filter_sub subs, None))
    | Error x -> Error x
  ;;

  (** [run query] executes [query]. 
      Returns pair of substitions and optionally a function, 
      that will start backtracking when called. *)
  let run query =
    let query_term = Parser.parse_query query in
    match query_term with
    | Error _ -> critical "Cannot parse the query"
    | Ok root_goal ->
      let filter_sub sub =
        match unify sub with
        | Ok unifier ->
          if not debug
          then
            filter_substitution
              (unifier @ swap_vars unifier)
              (get_vars_from_term root_goal)
          else unifier @ swap_vars unifier
        | Error _ -> sub
      in
      (match eval root_goal [] [] with
       | Ok (subs, choicepoints) ->
         let search_tree_depleted = is_empty choicepoints in
         if not search_tree_depleted
         then
           return
             (InterpretationResult
                (filter_sub subs, Some (fun () -> run_backtrack filter_sub choicepoints)))
         else return (InterpretationResult (filter_sub subs, None))
       | Error x -> fail x)
  ;;
end
