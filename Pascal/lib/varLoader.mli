(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

(** Create world from def list.
    Fills created variables with std values.*)
val load_variables : define list -> world
