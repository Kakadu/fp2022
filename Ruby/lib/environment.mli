(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module State : sig
  type storage

  val create : storage
  val add_state : global:storage -> local:storage -> storage
  val get_variable : storage -> string -> Ast.value
  val set_variable : storage -> string -> Ast.value -> storage
end
