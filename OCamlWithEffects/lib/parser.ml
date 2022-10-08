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
  fix
  @@ fun self ->
  remove_spaces
  *> (parens self
     <|>
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
     parse_literal >>| fun x -> ELiteral x)
;;

let%test _ =
  parse_string ~consume:Prefix parse_literal "\n \t (888)"
  = Result.ok @@ ELiteral (LInt 888)
;;

let%test _ =
  parse_string ~consume:Prefix parse_literal "  ((888))"
  = Result.ok @@ ELiteral (LInt 888)
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

let parse_entity =
  fix
  @@ fun self ->
  remove_spaces
  *> (parens self
     <|> take_while1 (fun x ->
           contains "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'_" x)
     )
;;

let%test _ = parse_string ~consume:Prefix parse_entity "_ -> x" = Result.ok @@ "_"
let%test _ = parse_string ~consume:Prefix parse_entity "  add x y" = Result.ok @@ "add"

let parse_identifier = parse_entity >>| fun x -> EIdentifier x

let%test _ =
  parse_string ~consume:Prefix parse_identifier " (y) " = Result.ok @@ EIdentifier "y"
;;

let%test _ =
  parse_string ~consume:Prefix parse_identifier "  f x" = Result.ok @@ EIdentifier "f"
;;

let parse_tuple =
  fix
  @@ fun self ->
  remove_spaces
  *> (parens self
     <|>
     let parse_content = choice [ parse_literal; parse_identifier; self ] in
     parse_content
     <* remove_spaces
     <* char ','
     <* remove_spaces
     >>= fun h ->
     sep_by1 (remove_spaces <* char ',' *> remove_spaces) parse_content
     <* remove_spaces
     >>| fun l -> ETuple (h :: l))
;;

(* Elements of a list can also be functions
   However, just adding `parse_function` to `choice` won't help,
   since `parse_variable` and `parse_function` work in same way,
   that is, by using `parse_entity`, so the parse result will
   either be ambiguous, or it will always be EVariable. *)
let parse_list =
  fix
  @@ fun self ->
  let brackets parser = char '[' *> parser <* char ']'
  and parse_content =
    choice [ parse_literal; parse_identifier; self; parens parse_tuple ]
  in
  remove_spaces
  *> (parens self
     <|> (brackets
          @@ (remove_spaces
             *> many
                  (parse_content
                  <* remove_spaces
                  <* (char ';' *> remove_spaces <|> remove_spaces)))
         >>| fun l -> EList l))
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
  parse_string ~consume:Prefix parse_list "  [ 'h' ; 'e' ; 'l' ; 'l' ; 'o'  ]   "
  = Result.ok
    @@ EList
         [ ELiteral (LChar 'h')
         ; ELiteral (LChar 'e')
         ; ELiteral (LChar 'l')
         ; ELiteral (LChar 'l')
         ; ELiteral (LChar 'o')
         ]
;;

let%test _ =
  parse_string ~consume:Prefix parse_list " [1  ]   "
  = Result.ok @@ EList [ ELiteral (LInt 1) ]
;;

let%test _ =
  parse_string ~consume:Prefix parse_list "[ 1; 2; 3]"
  = Result.ok @@ EList [ ELiteral (LInt 1); ELiteral (LInt 2); ELiteral (LInt 3) ]
;;

let%test _ =
  parse_string ~consume:Prefix parse_list "[ 1; [true; false]; (())]"
  = Result.ok
    @@ EList
         [ ELiteral (LInt 1)
         ; EList [ ELiteral (LBool true); ELiteral (LBool false) ]
         ; ELiteral LUnit
         ]
;;

let%test _ =
  parse_string ~consume:Prefix parse_list "[ 1; [true; false]; ('s', 'e'); ]"
  = Result.ok
    @@ EList
         [ ELiteral (LInt 1)
         ; EList [ ELiteral (LBool true); ELiteral (LBool false) ]
         ; ETuple [ ELiteral (LChar 's'); ELiteral (LChar 'e') ]
         ]
;;

let%test _ =
  parse_string ~consume:Prefix parse_list "[ (); (()); (1); (x, y)]"
  = Result.ok
    @@ EList
         [ ELiteral LUnit
         ; ELiteral LUnit
         ; ELiteral (LInt 1)
         ; ETuple [ EIdentifier "x"; EIdentifier "y" ]
         ]
;;

let%test _ =
  parse_string ~consume:Prefix parse_list "[ (a, b); (5, c, (d, e))]"
  = Result.ok
    @@ EList
         [ ETuple [ EIdentifier "a"; EIdentifier "b" ]
         ; ETuple
             [ ELiteral (LInt 5)
             ; EIdentifier "c"
             ; ETuple [ EIdentifier "d"; EIdentifier "e" ]
             ]
         ]
;;

(* let%test _ =
  parse_string ~consume:Prefix parse_list "[ (a, b); ((d, e), 5, c); ]"
  = Result.ok
    @@ EList
         [ ETuple [ EIdentifier "a"; EIdentifier "b" ]
         ; ETuple
             [ ETuple [ EIdentifier "d"; EIdentifier "e" ]
             ; ELiteral (LInt 5)
             ; EIdentifier "c"
             ]
         ]
;; *)

(* let%test _ =
  parse_string ~consume:Prefix parse_list "[ (((), ())); ((a, b), ) ]"
  = Result.ok
    @@ EList
         [ ETuple [ EIdentifier "a"; EIdentifier "b" ]
         ; ETuple
             [ ELiteral (LInt 5)
             ; EIdentifier "c"
             ; ETuple [ EIdentifier "d"; EIdentifier "e" ]
             ]
         ]
;; *)

let parse_fun =
  fix
  @@ fun self ->
  remove_spaces
  *> (parens self
     <|> (string "fun"
          *> many1 (parse_entity >>= fun v -> if v = "->" then fail "" else return v)
         <* remove_spaces
         <* string "->"
         <* remove_spaces
         >>= fun var_list ->
         choice [ self; parse_literal; parse_list; parse_identifier ]
         >>| fun expression -> EFun (var_list, expression)))
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
  = Result.ok @@ EFun ([ "x"; "y"; "_" ], EList [ EIdentifier "x"; EIdentifier "y" ])
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
             ; EIdentifier "par1"
             ; ELiteral (LString "line 2")
             ; EIdentifier "par2"
             ; ELiteral (LString "line 3")
             ] )
;;

(* parse_tuple was here!!! *)

let%test _ =
  parse_string ~consume:Prefix parse_tuple "  ( 1 , '2' , \"3\" )  "
  = Result.ok
    @@ ETuple [ ELiteral (LInt 1); ELiteral (LChar '2'); ELiteral (LString "3") ]
;;

let%test _ =
  parse_string ~consume:Prefix parse_tuple " x,y , z "
  = Result.ok @@ ETuple [ EIdentifier "x"; EIdentifier "y"; EIdentifier "z" ]
;;

let parse = Error "TODO"