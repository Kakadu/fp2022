(** Copyright 2021-2022, Chizhov Anton *)

(** SPDX-License-Identifier: CC0-1.0 *)

(* Checks if one of predicates is true *)
val some_pred : ('a -> bool) list -> 'a -> bool

(* Checks if all predicates are true *)
val all_pred : ('a -> bool) list -> 'a -> bool

(* Computes diff between l1 and l2 *)
val diff : 'a list -> 'a list -> 'a list

(* standard file descriptors (for writing more readable code) *)
type std_fd =
  | StdIn (* 0 *)
  | StdOut (* 1 *)
  | StdErr (* 2 *)

(* get int by standard file descriptor *)
val fd_to_int : std_fd -> int

(* List of standart fds *)
val std_fds : Unix.file_descr list

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
