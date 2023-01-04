(** Copyright 2021-2023, andreyizrailev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast.Ast(MonadError.Result)

(* Parse the whole NASM program and produce the AST *)
val program_p : ast t
