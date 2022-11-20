(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

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
end

val ident_sign_to_string_sign : Ident.ident Ast.signature -> string Ast.signature
