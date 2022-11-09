(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(**
Holder for immutable arrays.
Does not actually construct array, while it not modified.
*)
type 'a t =
  | Created of 'a Array.t
  | Empty of int * 'a

val length : 'a t -> int
val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> 'a t
val make : int -> 'a -> 'a t
val to_list : 'a t -> 'a list
val of_list : 'a list -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val fold_right : ('b -> 'a -> 'a) -> 'b t -> 'a -> 'a
val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
