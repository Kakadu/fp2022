(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type 'v t

val create : unit -> 'v t
val send : 'v t -> 'v -> unit
val receive : 'v t -> 'v
