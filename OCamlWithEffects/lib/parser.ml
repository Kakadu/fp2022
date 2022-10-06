open Angstrom
open Ast
open List
open String

type input = string
type error = string

let space_predicate x = x == ' ' || x == '\n' || x == '\t' || x == '\r'
let remove_spaces = take_while space_predicate

let%test _ =
  parse_string ~consume:Prefix (remove_spaces *> many any_char) "   \t\n\r   s"
  = Result.ok @@ [ 's' ]
;;

let parse_literal =
  let digit =
    satisfy (function
      | '0' .. '9' -> true
      | _ -> false)
  in
  let parse_int_literal =
    many1 digit
    >>| (fun x -> String.of_seq (List.to_seq x))
    >>| int_of_string
    >>| fun x -> LInt x
  and parse_string_literal =
    char '"' *> take_while (fun c -> c != '"') <* char '"' >>| fun x -> LString x
  and parse_char_literal = char '\'' *> any_char <* char '\'' >>| fun x -> LChar x
  and parse_bool_literal =
    string "true" <|> string "false" >>| bool_of_string >>| fun x -> LBool x
  and parse_unit_literal = string "()" >>| fun _ -> LUnit in
  remove_spaces
  *> choice
       [ parse_int_literal
       ; parse_string_literal
       ; parse_char_literal
       ; parse_bool_literal
       ; parse_unit_literal
       ]
  >>| fun x -> ELiteral x
;;

let%test _ =
  parse_string ~consume:Prefix parse_literal "  888   " = Result.ok @@ ELiteral (LInt 888)
;;

let%test _ =
  parse_string ~consume:Prefix parse_literal "\"Hello, world!\""
  = Result.ok @@ ELiteral (LString "Hello, world!")
;;

let%test _ =
  parse_string ~consume:Prefix parse_literal "   \'h\'\n\n\n   \t"
  = Result.ok @@ ELiteral (LChar 'h')
;;

let%test _ =
  parse_string ~consume:Prefix parse_literal "\n true"
  = Result.ok @@ ELiteral (LBool true)
;;

let%test _ =
  parse_string ~consume:Prefix parse_literal "false or true"
  = Result.ok @@ ELiteral (LBool false)
;;

let%test _ =
  parse_string ~consume:Prefix parse_literal " () " = Result.ok @@ ELiteral LUnit
;;

let parse_entity =
  remove_spaces
  *> take_while1 (fun x ->
       contains "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'_" x)
;;

let%test _ = parse_string ~consume:Prefix parse_entity "_ -> x" = Result.ok @@ "_"
let%test _ = parse_string ~consume:Prefix parse_entity "  add x y" = Result.ok @@ "add"

let parse_variable = parse_entity >>| fun x -> EVariable x

let%test _ = parse_string ~consume:Prefix parse_variable "y" = Result.ok @@ EVariable "y"

let parse_function = parse_entity >>| fun x -> EFunction x

let%test _ =
  parse_string ~consume:Prefix parse_function "f x" = Result.ok @@ EFunction "f"
;;

let parse = Error ""