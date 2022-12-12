(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module type MONADERROR = sig
  type error =
    | ParseError of string
    | TypeError of string
    | RuntimeError of string

  type 'a t = ('a, error) result

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t
end
(* type error =
    | ParseError of string
    | TypeError of string
    | RuntimeError of string

  type 'a t = ('a, error) result

  let ( >>= ) func = function
    | Ok x -> func x
    | Error s -> Error s
  ;;

  let return x = Ok x
  let fail error = Error error
end *)