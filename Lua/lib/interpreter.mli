(** Copyright 2021-2022, Arthur Alekseev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Interpreter : sig
  type context

  type result =
    | Done of context
    | Fail of string

  val interpret : Ast.Ast.ast -> context -> result
  val emptyctx : context
end
