(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

val get_type_val : value -> vtype
(** Get value type. *)

val compare_types : vtype -> vtype -> bool
(** Compare types except types with expr in definition. *)
