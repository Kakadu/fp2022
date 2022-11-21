(** Copyright 2021-2022, ol-imorozko and contributors *)

(** Main entry of parser *)
val parse : string -> (Ast.ast, string) result
