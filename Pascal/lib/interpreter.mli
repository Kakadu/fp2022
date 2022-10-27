(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type interpret_result = Succes of world | Fail of exn

val interpret : string -> interpret_result
(** parse -> semantic test -> interpret *)
