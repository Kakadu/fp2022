(** Copyright 2021-2022, Chizhov Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Utils

module type MonadFail = sig
  include Base.Monad.Infix

  val return : 'a -> 'a t
  val fail : string -> 'a t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val ( *> ) : 'a t -> 'b -> 'b t
  val ( <* ) : 'a t -> 'b -> 'a t
end

module Result : sig
  type 'a t = ('a, string) result

  val return : 'a -> 'a t
  val fail : string -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val ( *> ) : 'a t -> 'b -> 'b t
  val ( <* ) : 'a t -> 'b -> 'a t
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
    ; functions : script StrMap.t
    ; retcode : int
    ; fds : Unix.file_descr IntMap.t
    }
  [@@deriving show { with_path = false }]

  val default_env : environment
  val eval : ?env:environment -> script -> environment M.t
end
