open Angstrom
open Ast
open List
open String

type input = string
(* type error = string *)

let space_predicate x = x == ' ' || x == '\n' || x == '\t' || x == '\r'
let remove_spaces = take_while space_predicate

let%test _ =
  parse_string ~consume:Prefix (remove_spaces *> many any_char) "   \t\n\r   s"
  = Result.ok @@ [ 's' ]
;;

let parens parser = char '(' *> parser <* char ')'

let parse_literal =
  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false
  in
  let parse_int_literal = take_while1 is_digit >>| int_of_string >>| fun x -> LInt x
  and parse_string_literal =
    char '"' *> take_while (( != ) '"') <* char '"' >>| fun x -> LString x
  and parse_char_literal = char '\'' *> any_char <* char '\'' >>| fun x -> LChar x
  and parse_bool_literal =
    string "true" <|> string "false" >>| bool_of_string >>| fun x -> LBool x
  and parse_unit_literal = string "()" >>| fun _ -> LUnit in
  let parse_literal =
    choice
      [ parse_int_literal
      ; parse_string_literal
      ; parse_char_literal
      ; parse_bool_literal
      ; parse_unit_literal
      ]
  in
  remove_spaces *> choice [ parens parse_literal; parse_literal ] >>| fun x -> ELiteral x
;;

let%test _ =
  parse_string ~consume:Prefix parse_literal "(888)" = Result.ok @@ ELiteral (LInt 888)
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

let%test _ =
  match parse_string ~consume:Prefix parse_literal "x" with
  | Result.Ok _ -> false
  | _ -> true
;;

let%test _ =
  match parse_string ~consume:Prefix parse_literal "let f x = f x" with
  | Result.Ok _ -> false
  | _ -> true
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

(*TODO: come up with something...*)
let remove_brackets =
  char '(' *> many any_char >>| fun x -> String.of_seq (List.to_seq @@ rev @@ tl @@ rev x)
;;

let%test _ = parse_string ~consume:Prefix remove_brackets "(3 + 5)" = Result.ok @@ "3 + 5"

(* Elements of a list can also be functions
   However, just adding `parse_function` to `choice` won't help,
   since `parse_variable` and `parse_function` work in same way,
   that is, by using `parse_entity`, so the parse result will
   either be ambiguous, or it will always be EVariable. *)
let parse_list =
  let parse_content = choice [ parse_literal; parse_variable ] in
  let parse_elem_in_brackets =
    remove_spaces *> parse_content <* remove_spaces <* many (char ';')
  in
  let parse_in_brackets =
    char '[' *> many parse_elem_in_brackets <* remove_spaces <* char ']'
  in
  let parse_elem_in_constructor =
    parse_content <* remove_spaces <* string "::" <* remove_spaces
  in
  remove_spaces *> many parse_elem_in_constructor
  >>= fun x -> parse_in_brackets >>| (fun y -> append x y) >>| fun l -> EList l
;;

let%test _ =
  parse_string ~consume:Prefix parse_list "[ 1; 3; 7; ]"
  = Result.ok @@ EList [ ELiteral (LInt 1); ELiteral (LInt 3); ELiteral (LInt 7) ]
;;

let%test _ =
  parse_string
    ~consume:Prefix
    parse_list
    "   [\"apple\";\"orange\";\"banana\";\"pear\"]   "
  = Result.ok
    @@ EList
         [ ELiteral (LString "apple")
         ; ELiteral (LString "orange")
         ; ELiteral (LString "banana")
         ; ELiteral (LString "pear")
         ]
;;

let%test _ =
  parse_string ~consume:Prefix parse_list "   'h' :: 'e' :: 'l' :: [ 'l' ; 'o' ]   "
  = Result.ok
    @@ EList
         [ ELiteral (LChar 'h')
         ; ELiteral (LChar 'e')
         ; ELiteral (LChar 'l')
         ; ELiteral (LChar 'l')
         ; ELiteral (LChar 'o')
         ]
;;

let parse_fun =
  remove_spaces
  *> string "fun"
  *> many1 (parse_entity >>= fun v -> if v = "->" then fail "" else return v)
  <* remove_spaces
  <* string "->"
  <* remove_spaces
  >>= fun var_list ->
  let parse_fun = choice [ parse_literal; parse_list; parse_variable ] in
  choice [ parens parse_fun; parse_fun ] >>| fun expression -> EFun (var_list, expression)
;;

let%test _ =
  parse_string ~consume:Prefix parse_fun "fun x -> (888)"
  = Result.ok @@ EFun ([ "x" ], ELiteral (LInt 888))
;;

let%test _ =
  parse_string ~consume:Prefix parse_fun "fun _ -> (fun _ -> (\"Hello\"))"
  = Result.ok @@ EFun ([ "_" ], EFun ([ "_" ], ELiteral (LString "Hello")))
;;

let%test _ =
  parse_string ~consume:Prefix parse_fun "fun _ -> fun _ -> \"Hello\""
  = Result.ok @@ EFun ([ "_" ], EFun ([ "_" ], ELiteral (LString "Hello")))
;;

let%test _ =
  parse_string ~consume:Prefix parse_fun "((fun _ -> 5))"
  = Result.ok @@ EFun ([ "_" ], ELiteral (LInt 5))
;;

let%test _ =
  parse_string ~consume:Prefix parse_fun "fun x y _ -> [x; y]"
  = Result.ok @@ EFun ([ "x"; "y"; "_" ], EList [ EVariable "x"; EVariable "y" ])
;;

let%test _ =
  parse_string ~consume:Prefix parse_fun "fun _ -> 5"
  = Result.ok @@ EFun ([ "_" ], ELiteral (LInt 5))
;;

let%test _ =
  parse_string
    ~consume:Prefix
    parse_fun
    "   fun  par1  par2  ->  [\"line 1\"; par1; \"line 2\"; par2; \"line 3\"]   "
  = Result.ok
    @@ EFun
         ( [ "par1"; "par2" ]
         , EList
             [ ELiteral (LString "line 1")
             ; EVariable "par1"
             ; ELiteral (LString "line 2")
             ; EVariable "par2"
             ; ELiteral (LString "line 3")
             ] )
;;

(*TODO: brackets!!!!!!!*)

let parse = Error ""