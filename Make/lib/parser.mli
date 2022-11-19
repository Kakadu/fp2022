(** Main entry of parser *)
val parse : string -> (Ast.ast, string) result
