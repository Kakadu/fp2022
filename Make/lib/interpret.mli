(** Main entry of interpreter *)
val interpret : Ast.ast -> string list -> (string, string) result
