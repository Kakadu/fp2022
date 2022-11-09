(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

(** Get value type. *)
val get_type_val : value -> vtype

(** Compare types except types with expr in definition. *)
val compare_types : vtype -> vtype -> bool

val cast : vtype -> value -> value

(** cast_type : to -> from *)
val cast_type : vtype -> vtype -> bool
