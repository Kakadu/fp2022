(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type interpret_result =
  | Ok
  | FileNotFound
  | CE of string

val interpret : string -> interpret_result
