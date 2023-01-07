(** Copyright 2021-2022, Kakadu, EmirVildanov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typing
open Ast

type identifier = string

type error =
  [ `Occurs_check
  | `NoVariable of identifier
  | `UnificationFailed of Typing.t * Typing.t
  | `NotReachable
  | `NotImplementedYet
  ]

val run_inference : expr -> (t, error) result
val print_type_error : error -> unit
