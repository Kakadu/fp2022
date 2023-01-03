(** Copyright 2021-2022, Ilya Shchuckin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Defines main types that are used in the interpreter. *)

open Ast

(** [Critical] error forces program to stop it's calculations, 
    while [EvalError] is the typical failure of goals evaluation. *)
type failure =
  | Critical of string
  | EvalError of string

let failure_to_string = function
  | Critical x | EvalError x -> x
;;

type 'a result =
  | Ok of 'a
  | Error of failure

module type MonadFail = sig
  val return : 'a -> 'a result
  val fail : failure -> 'a result
  val ( >>| ) : 'a result -> ('a -> 'b) -> 'b result
  val ( >>= ) : 'a result -> ('a -> 'b result) -> 'b result
  val ( <|> ) : 'a result -> (unit -> 'a result) -> 'a result
  val ( *> ) : 'a result -> 'b result -> 'b result
  val lift2 : ('a -> 'b -> 'c) -> 'a result -> 'b result -> 'c result
end

module Result : MonadFail = struct
  let return x = Ok x
  let fail (x : failure) = Error x

  let ( >>= ) f g =
    match f with
    | Ok x -> g x
    | Error _ as e -> e
  ;;

  let ( >>| ) f g = f >>= fun x -> return (g x)

  let ( <|> ) f g =
    match f with
    | Ok _ | Error (Critical _) -> f
    | Error (EvalError _) -> g ()
  ;;

  let ( *> ) f g = f >>= fun _ -> g
  let lift2 f x y = x >>= fun res1 -> y >>= fun res2 -> return (f res1 res2)
end

type clause =
  { head : term
  ; goal : term
  }
[@@deriving show { with_path = false }]

type db = clause list [@@deriving show { with_path = false }]

(** Contains database and execution mode (debug) for the current run. *)
module type Config = sig
  (** If true enables additional info prints for debugging. *)
  val debug : bool

  (** Database constructed from prolog text. *)
  val db : db
end

type unifier = (term * term) list [@@deriving show { with_path = false }]

(** Defines three possible types of a choicepoint. 
    Type of a choicepoints depends on the predicate 
    while executing which it was created. *)
type choicepoint =
  | Choicepoint of
      { goal : term
      ; substitution : unifier
      ; candidates : clause list
      }
  | Conjuction of
      { goal : term
      ; choicepoints : choicepoints
      ; substitution : unifier
      }
  | Clause of
      { substitution : unifier
      ; candidates : clause list
      ; head : term
      ; body : term
      }
[@@deriving show { with_path = false }]

and choicepoints = choicepoint list [@@deriving show { with_path = false }]

(** Consists of substitution and optionally a function that starts backtracking. *)
type interp_res = InterpretationResult of (unifier * (unit -> interp_res result) option)
