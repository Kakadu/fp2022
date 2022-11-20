open Ast
open Ident

val lookup : string list -> string source_file -> (ident source_file, string list) result
