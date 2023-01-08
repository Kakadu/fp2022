(** Copyright 2022-2023, Denis Porsev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  | ParseError of string
  | TypeError of string
  | RuntimeError of string

module type MONADERROR = sig
  type 'a t = ('a, error) result

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t
end
