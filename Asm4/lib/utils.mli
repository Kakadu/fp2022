(** Copyright 2021-2023, andreyizrailev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module ListStack : sig
  type 'a t

  val empty : 'a t
  val push : 'a -> 'a t -> 'a t
  val peek : 'a t -> 'a option
  val pop : 'a t -> 'a t option
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module List : sig
  include module type of Stdlib.List

  val index_of_elem : 'a -> ('a -> 'a -> bool) -> 'a t -> int option
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

(* Map with int keys *)
module IntMap : sig
  include Map.S with type key = int

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

(* Map with string keys *)
module StringMap : sig
  include Map.S with type key = string

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end
