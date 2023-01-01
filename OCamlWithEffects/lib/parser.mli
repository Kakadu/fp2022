type error_message = string
type input = string

val parse : input -> (Ast.expression list, error_message) result
