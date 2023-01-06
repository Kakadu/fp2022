(** Copyright 2021-2023, andreyizrailev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open MonadError

module Transformations (M : MonadError) : sig
  open Ast.Ast(M)

  val transform_whole : ast -> ast M.t
end
