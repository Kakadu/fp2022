(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

type interpret_result =
  | Ok
  | FileNotFound
  | CE of string

val interpret : string -> interpret_result
