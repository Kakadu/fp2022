(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Utils

module type MonadFail = sig
  include Base.Monad.Infix

  val return : 'a -> 'a t
  val fail : string -> 'a t
  val ( <|> ) : 'a t -> (unit -> 'a t) -> 'a t
end

module Result : sig
  type 'a t = ('a, string) result

  val return : 'a -> 'a t
  val fail : string -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( <|> ) : 'a t -> (unit -> 'a t) -> 'a t
end

module Interpret (M : MonadFail) : sig
  (* All variables in Bash are arrays.
     Simple variables are indexed arrays *)
  type variable =
    | IndexedArray of string IntMap.t
    | AssocArray of string StrMap.t
  [@@deriving variants, show { with_path = false }]

  type environment =
    { vars : variable StrMap.t
    ; functions : group StrMap.t
    ; retcode : int
    }
  [@@deriving show { with_path = false }]

  val eval : script -> environment M.t
end
