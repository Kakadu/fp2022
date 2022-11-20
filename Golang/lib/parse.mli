(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

open Ast

val parse_file : string -> (string source_file, string) result
