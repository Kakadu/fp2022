(* type input = string
type error = string *)

(* open Ast *)

val parse_list : Ast.expression Angstrom.t
val parse_tuple : Ast.expression Angstrom.t
val parse_literal : Ast.expression Angstrom.t
val parse_identifier : Ast.expression Angstrom.t
val parse_fun : Ast.expression Angstrom.t
val parse_declaration : Ast.expression Angstrom.t
val parse_expression : Ast.expression Angstrom.t
val parse_conditional : Ast.expression Angstrom.t
val parse_expression : Ast.expression Angstrom.t
val parse_application : Ast.expression Angstrom.t
val parse_binary_operation : Ast.expression Angstrom.t
val parse_matching : Ast.expression Angstrom.t
val parse_unary_operation : Ast.expression Angstrom.t
val parse_list_constructing : Ast.expression Angstrom.t
val parse_data_constructor : Ast.expression Angstrom.t
val parens : 'a Angstrom.t -> 'a Angstrom.t

(* val parse : string -> (expression, string) result *)