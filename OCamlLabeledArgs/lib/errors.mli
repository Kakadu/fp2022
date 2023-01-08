(** Copyright 2022-2023, Denis Porsev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** [Errors] contains definition of errors in our mini-language. *)

type error =
  | ParseError of string (** [ParseError] happens on parsing stage *)
  | TypeError of string (** [TypeError] happens on inference stage *)
  | RuntimeError of string (** [RuntimeError] happens on evaluation stage *)

module type MONADERROR = sig
  type 'a t = ('a, error) result

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t
end
