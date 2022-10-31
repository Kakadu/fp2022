(** Copyright 2021-2022, Chizhov Anton *)

(** SPDX-License-Identifier: CC0-1.0 *)

(* Checks if one of predicates is true *)
val some_pred : ('a -> bool) list -> 'a -> bool

(* Computes diff between l1 and l2 *)
val diff : 'a list -> 'a list -> 'a list

(* Map with string keys *)
module StrMap : sig
  include Map.S with type key = string

  val from_list : (key * 'a) list -> 'a t
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

(* Map with integer keys *)
module IntMap : sig
  include Map.S with type key = int

  val from_list : 'a list -> 'a t
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end
