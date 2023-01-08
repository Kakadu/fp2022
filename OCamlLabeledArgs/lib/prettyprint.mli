(** Copyright 2022-2023, Denis Porsev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Pretty printing for [ast] types defined in [parsetree] and [typedtree],
    [error] type defined in [errors] and [environment]. *)

(** [pp_expr] prints [expr] type from [parsetree] using [Format.formatter] *)
val pp_expr : Format.formatter -> Parsetree.expr -> unit

(** [pp_typ] prints [typ] type from [typedtree] using [Format.formatter] *)
val pp_typ : Format.formatter -> Typedtree.typ -> unit

(** [pp_value] prints [value] type using [Format.formatter] *)
val pp_value : Format.formatter -> Typedtree.value -> unit

(** [pp_error] prints [error] type using [Format.formatter] *)
val pp_error : Format.formatter -> Errors.error -> unit

(** [pp_env] prints [environment] type using [Format.formatter] *)
val pp_env : Format.formatter -> Typedtree.environment -> unit
