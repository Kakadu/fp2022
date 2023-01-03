(** Copyright 2022-2023, Denis Porsev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Module [Interpret] is responsible for evalution of expressions. *)

module Interpret : functor (M : Errors.MONADERROR) -> sig
  (** [compare_values] is called for [Typedtree.value]s comparisons *)
  val compare_values : Typedtree.value -> Typedtree.value -> int M.t

  (** [eval] is the entry point of [Interpret]or *)
  val eval : Parsetree.expr -> Typedtree.environment -> Typedtree.value M.t
end

module EvalResult : sig
  type 'a t = ('a, Errors.error) result

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : Errors.error -> 'a t
end
