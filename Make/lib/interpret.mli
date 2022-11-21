(** Copyright 2021-2022, ol-imorozko and contributors *)

(** Main entry of interpreter *)
val interpret : Ast.ast -> string list -> (string, string) result
