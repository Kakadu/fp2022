(** Copyright 2021-2022, Startsev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >> ) : 'a t -> 'b t -> 'b t
end

module type MONADERROR = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >> ) : 'a t -> 'b t -> 'b t
  val error : string -> 'a t
end

module Result : sig
  type 'a t = ('a, string) Result.t

  val ( >>= ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
  val ( >> ) : ('a, 'b) result -> ('c, 'b) result -> ('c, 'b) result
  val return : 'a -> ('a, 'b) result
  val error : 'a -> ('b, 'a) result
end

module Interpret : functor (M : MONADERROR) -> sig
  module MapVar : sig
    type key = string
    type 'a t = 'a Map.Make(String).t

    val empty : 'a t
  end

  type var = Reg64 of int | Reg128 of string | Const of string
  type envr = var MapVar.t

  val show_envr : envr -> string
  val r_list : (string * var) list
  val prep : (MapVar.key * 'a) list -> 'a MapVar.t
  val interpret : var MapVar.t -> ast -> var MapVar.t M.t
end
