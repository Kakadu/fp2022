(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Ident

module type State = sig
  type t
end

module Pass (S : State) : sig
  type 'r t

  (* Run *)

  val run_pass : 'a t -> init:S.t -> S.t * 'a

  (* Combinators *)

  val access : S.t t
  val put : S.t -> unit t
  val return : 'r -> 'r t
  val bind : 'r t -> ('r -> 'rr t) -> 'rr t
  val ( let* ) : 'r t -> ('r -> 'rr t) -> 'rr t

  (* High-level combinators *)
  val ignore : 'a t -> unit t
  val ( *> ) : 'a t -> 'b t -> 'b t
  val ( <* ) : 'a t -> 'b t -> 'a t
  val many : 'a list -> f:('a -> 'b t) -> 'b list t
  val fold_state : 'a list -> f:('a -> unit t) -> unit t
end = struct
  type 'r t = S.t -> S.t * 'r

  (* Run *)

  let run_pass t ~init = t init

  (* Combinators *)

  let access s = s, s
  let put s _ = s, ()
  let return r s = s, r

  let bind t f s =
    let s, r = t s in
    let f_t = f r in
    f_t s
  ;;

  let ( let* ) = bind

  (* High-level combinators *)

  let ( *> ) a b =
    let* _ = a in
    b
  ;;

  let ignore t = t *> return ()

  let ( <* ) a b =
    let* a = a in
    let* _ = b in
    return a
  ;;

  let many xs ~f =
    let rec helper acc xs =
      let* ys = acc in
      match xs with
      | [] -> return (List.rev ys)
      | h :: tl ->
        let* y = f h in
        helper (return (y :: ys)) tl
    in
    helper (return []) xs
  ;;

  let fold_state xs ~f =
    let rec helper acc = function
      | [] -> acc
      | h :: tl -> helper (acc *> f h) tl
    in
    helper (return ()) xs
  ;;
end

let ident_sign_to_string_sign { args; ret } =
  let args = List.map args ~f:(fun (id, t) -> name id, t) in
  { args; ret }
;;
