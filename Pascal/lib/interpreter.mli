(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

(** parse -> semantic test -> interpret *)
val interpret_no_catch : string -> world

(** parse -> semantic test -> interpret *)
val interpret : string -> (world, exn) result
