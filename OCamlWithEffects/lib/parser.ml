open Angstrom
open Ast
open List
open String

type input = string

type dispatch =
  { parse_tuple : dispatch -> expression Angstrom.t
  ; parse_list : dispatch -> expression Angstrom.t
  ; parse_fun : dispatch -> expression Angstrom.t
  ; parse_expression : dispatch -> expression Angstrom.t
  ; parse_declaration : dispatch -> expression Angstrom.t
  ; parse_conditional : dispatch -> expression Angstrom.t
  ; parse_matching : dispatch -> expression Angstrom.t
  ; parse_binary_operation : dispatch -> expression Angstrom.t
  }

(* Functions for constructing expressions *)
let eliteral x = ELiteral x
let eidentifier x = EIdentifier x
let etuple head tail = ETuple (head :: tail)
let elist x = EList x
let efun variable_list expression = EFun (variable_list, expression)

let ebinary_operation operator left_operand right_operand =
  EBinaryOperation (operator, left_operand, right_operand)
;;

let edeclaration function_name variable_list expression =
  EDeclaration (function_name, variable_list, expression)
;;

let eif condition true_branch false_branch = EIf (condition, true_branch, false_branch)
let ematchwith expression cases = EMatchWith (expression, cases)

(* Binary operators smart constructors *)
let badd _ = Add
let bsub _ = Sub
let bmul _ = Mul
let bdiv _ = Div
let beq _ = Eq
let bneq _ = NEq
let bgt _ = GT
let bgte _ = GTE
let blt _ = LT
let blte _ = LTE
let band _ = AND
let bor _ = OR

(* -------------------------------------- *)

let space_predicate x = x == ' ' || x == '\n' || x == '\t' || x == '\r'
let remove_spaces = take_while space_predicate

let%test _ =
  parse_string ~consume:Prefix (remove_spaces *> many any_char) "   \t\n\r   s"
  = Result.ok @@ [ 's' ]
;;

let parens parser = remove_spaces *> char '(' *> parser <* char ')'

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
     lift eliteral parse_literal)
;;

(* Tests for literal parsing *)
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

(* Tests for entity parsing *)
let%test _ = parse_string ~consume:Prefix parse_entity "_ -> x" = Result.ok @@ "_"
let%test _ = parse_string ~consume:Prefix parse_entity "  add x y" = Result.ok @@ "add"

let keywords = [ "let"; "rec"; "match"; "with"; "if"; "then"; "in"; "fun" ]

let parse_identifier =
  parse_entity
  >>= fun entity ->
  if List.exists (( = ) entity) keywords
  then fail "Keyword used!"
  else return @@ eidentifier entity
;;

(* Tests for parsing identifiers *)
let%test _ =
  parse_string ~consume:Prefix parse_identifier " (y) " = Result.ok @@ EIdentifier "y"
;;

let%test _ =
  parse_string ~consume:Prefix parse_identifier "  f x" = Result.ok @@ EIdentifier "f"
;;

(* Write new parsers (except tuple) here! *)
let parsers_except d =
  [ parse_literal
  ; d.parse_fun d
  ; d.parse_list d
  ; d.parse_conditional d
  ; d.parse_matching d
  ; parse_identifier
  ]
;;

(* -------------------------------------- *)

let parse_tuple d =
  fix
  @@ fun self ->
  remove_spaces
  *> ((let parse_content = choice @@ parsers_except d <|> parens self
       and separator = remove_spaces *> char ',' *> remove_spaces in
       lift2
         etuple
         (parse_content <* separator)
         (sep_by1 separator parse_content <* remove_spaces))
     <|> parens self)
;;

let parse_list d =
  fix
  @@ fun self ->
  remove_spaces
  *>
  let brackets parser = char '[' *> parser <* char ']'
  and separator = remove_spaces *> char ';' *> remove_spaces <|> remove_spaces in
  parens self
  <|> lift elist @@ brackets @@ (remove_spaces *> many (d.parse_expression d <* separator))
;;

let parse_fun d =
  fix
  @@ fun self ->
  remove_spaces
  *> (parens self
     <|> string "fun"
         *> lift2
              efun
              (many1 (parse_entity >>= fun v -> if v = "->" then fail "" else return v)
              <* remove_spaces
              <* string "->"
              <* remove_spaces)
              (d.parse_expression d))
;;

let parse_declaration d =
  remove_spaces
  *> string "let"
  *> lift3
       edeclaration
       parse_entity
       (many1 (parse_entity >>= fun v -> if v = "=" then fail "" else return v))
       (remove_spaces *> string "=" *> d.parse_expression d)
;;

let parse_conditional d =
  fix
  @@ fun self ->
  remove_spaces
  *> (parens self
     <|> string "if"
         *> lift3
              eif
              (d.parse_expression d)
              (remove_spaces *> string "then" *> d.parse_expression d)
              (remove_spaces *> string "else" *> d.parse_expression d))
;;

let parse_matching d =
  fix
  @@ fun self ->
  remove_spaces
  *> (parens self
     <|> string "match"
         *> lift2
              ematchwith
              (d.parse_expression d)
              (let parse_case =
                 lift2
                   (fun case action -> case, action)
                   (d.parse_expression d)
                   (remove_spaces *> string "->" *> d.parse_expression d)
               and separator = remove_spaces *> string "|" in
               remove_spaces
               *> string "with"
               *> remove_spaces
               *> (string "|" <|> remove_spaces)
               *> sep_by1 separator parse_case))
;;

let parse_binary_operation d =
  fix
  @@ fun self ->
  remove_spaces
  *>
  let multiplicative = remove_spaces *> choice [ char '*' >>| bmul; char '/' >>| bdiv ]
  and additive = remove_spaces *> choice [ char '+' >>| badd; char '-' >>| bsub ]
  and relational =
    remove_spaces
    *> choice
         [ string ">=" >>| bgte
         ; string "<=" >>| blte
         ; char '>' >>| bgt
         ; char '<' >>| blt
         ]
  and equality =
    remove_spaces *> choice [ string "==" >>| beq; string "!=" <|> string "<>" >>| bneq ]
  and logical_and = remove_spaces *> (string "&&" >>| band)
  and logical_or = remove_spaces *> (string "||" >>| bor) in
  let chainl1 expression_parser operation_parser =
    let rec go acc =
      lift2
        (fun binary_operator right_operand ->
          ebinary_operation binary_operator acc right_operand)
        operation_parser
        expression_parser
      >>= go
      <|> return acc
    in
    expression_parser >>= fun init -> go init
  in
  let ( <||> ) = chainl1 in
  let parse_content = d.parse_tuple d <|> parens self <|> choice (parsers_except d) in
  parse_content
  <||> multiplicative
  <||> additive
  <||> relational
  <||> equality
  <||> logical_and
  <||> logical_or
;;

let parse_expression d =
  d.parse_tuple d <|> d.parse_binary_operation d <|> choice @@ parsers_except d
;;

let default =
  { parse_tuple
  ; parse_list
  ; parse_fun
  ; parse_expression
  ; parse_declaration
  ; parse_conditional
  ; parse_matching
  ; parse_binary_operation
  }
;;

let parse_tuple = default.parse_tuple default
let parse_list = default.parse_list default
let parse_fun = default.parse_fun default
let parse_expression = default.parse_expression default
let parse_declaration = default.parse_declaration default
let parse_conditional = default.parse_conditional default
let parse_matching = default.parse_matching default
let parse_binary_operation = default.parse_binary_operation default

(* Tests for list parsing *)
let%test _ =
  parse_string
    ~consume:Prefix
    parse_expression
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
  parse_string ~consume:Prefix parse_expression "  [ 'h' ; 'e' ; 'l' ; 'l' ; 'o'  ]   "
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
  parse_string ~consume:Prefix parse_expression " [1  ]   "
  = Result.ok @@ EList [ ELiteral (LInt 1) ]
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression "[ 1; 2; 3]"
  = Result.ok @@ EList [ ELiteral (LInt 1); ELiteral (LInt 2); ELiteral (LInt 3) ]
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression "[ 1; [true; false]; (())]"
  = Result.ok
    @@ EList
         [ ELiteral (LInt 1)
         ; EList [ ELiteral (LBool true); ELiteral (LBool false) ]
         ; ELiteral LUnit
         ]
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression "[ 1; [true; false]; ('s', 'e'); ]"
  = Result.ok
    @@ EList
         [ ELiteral (LInt 1)
         ; EList [ ELiteral (LBool true); ELiteral (LBool false) ]
         ; ETuple [ ELiteral (LChar 's'); ELiteral (LChar 'e') ]
         ]
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression "[ (); (()); (1); (x, y)]"
  = Result.ok
    @@ EList
         [ ELiteral LUnit
         ; ELiteral LUnit
         ; ELiteral (LInt 1)
         ; ETuple [ EIdentifier "x"; EIdentifier "y" ]
         ]
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression "[ (a, b); (5, c, (d, e))]"
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

let%test _ =
  parse_string ~consume:Prefix parse_expression "[ (a, b); ((d, e), 5, c); ]"
  = Result.ok
    @@ EList
         [ ETuple [ EIdentifier "a"; EIdentifier "b" ]
         ; ETuple
             [ ETuple [ EIdentifier "d"; EIdentifier "e" ]
             ; ELiteral (LInt 5)
             ; EIdentifier "c"
             ]
         ]
;;

let%test _ =
  parse_string
    ~consume:Prefix
    parse_expression
    "[ (); (()); (((), ())); [(); 1]; ('a', 'b')]"
  = Result.ok
    @@ EList
         [ ELiteral LUnit
         ; ELiteral LUnit
         ; ETuple [ ELiteral LUnit; ELiteral LUnit ]
         ; EList [ ELiteral LUnit; ELiteral (LInt 1) ]
         ; ETuple [ ELiteral (LChar 'a'); ELiteral (LChar 'b') ]
         ]
;;

let%test _ =
  match parse_string ~consume:Prefix parse_expression "[a, b), c]" with
  | Result.Ok _ -> false
  | _ -> true
;;

let%test _ =
  match parse_string ~consume:Prefix parse_expression "[a, (1, 2)), ()]" with
  | Result.Ok _ -> false
  | _ -> true
;;

(* Tests for lambda function parsing *)
let%test _ =
  parse_string ~consume:Prefix parse_expression "fun x -> (888)"
  = Result.ok @@ EFun ([ "x" ], ELiteral (LInt 888))
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression "fun _ -> (fun _ -> (\"Hello\"))"
  = Result.ok @@ EFun ([ "_" ], EFun ([ "_" ], ELiteral (LString "Hello")))
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression "fun _ -> fun _ -> \"Hello\""
  = Result.ok @@ EFun ([ "_" ], EFun ([ "_" ], ELiteral (LString "Hello")))
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression "((fun _ -> 5))"
  = Result.ok @@ EFun ([ "_" ], ELiteral (LInt 5))
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression "fun x y _ -> [x; y]"
  = Result.ok @@ EFun ([ "x"; "y"; "_" ], EList [ EIdentifier "x"; EIdentifier "y" ])
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression "fun _ -> 5"
  = Result.ok @@ EFun ([ "_" ], ELiteral (LInt 5))
;;

let%test _ =
  parse_string
    ~consume:Prefix
    parse_expression
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

(* Tests for tuple parsing *)
let%test _ =
  parse_string ~consume:Prefix parse_expression "  ( 1 , '2' , \"3\" )  "
  = Result.ok
    @@ ETuple [ ELiteral (LInt 1); ELiteral (LChar '2'); ELiteral (LString "3") ]
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression " x,y , z "
  = Result.ok @@ ETuple [ EIdentifier "x"; EIdentifier "y"; EIdentifier "z" ]
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression " (x,y) , z "
  = Result.ok @@ ETuple [ ETuple [ EIdentifier "x"; EIdentifier "y" ]; EIdentifier "z" ]
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression "(fun _ -> 1, fun _ -> 2)"
  = Result.ok
    @@ EFun ([ "_" ], ETuple [ ELiteral (LInt 1); EFun ([ "_" ], ELiteral (LInt 2)) ])
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression "[fun _ -> 1; fun _ -> 2]"
  = Result.ok
    @@ EList [ EFun ([ "_" ], ELiteral (LInt 1)); EFun ([ "_" ], ELiteral (LInt 2)) ]
;;

(* Tests for function declaration *)
let%test _ =
  parse_string ~consume:Prefix parse_declaration "let f x = ((x, [1; x]), true)"
  = Result.ok
    @@ EDeclaration
         ( "f"
         , [ "x" ]
         , ETuple
             [ ETuple [ EIdentifier "x"; EList [ ELiteral (LInt 1); EIdentifier "x" ] ]
             ; ELiteral (LBool true)
             ] )
;;

let%test _ =
  parse_string
    ~consume:Prefix
    parse_declaration
    "let f x = fun arg1 arg2 -> (arg1, fun x -> [arg1; x])"
  = Result.ok
    @@ EDeclaration
         ( "f"
         , [ "x" ]
         , EFun
             ( [ "arg1"; "arg2" ]
             , ETuple
                 [ EIdentifier "arg1"
                 ; EFun ([ "x" ], EList [ EIdentifier "arg1"; EIdentifier "x" ])
                 ] ) )
;;

let%test _ =
  parse_string
    ~consume:Prefix
    parse_declaration
    "let f x = (if x then (1, x) else (fun y -> [x; y]))"
  = Result.ok
    @@ EDeclaration
         ( "f"
         , [ "x" ]
         , EIf
             ( EIdentifier "x"
             , ETuple [ ELiteral (LInt 1); EIdentifier "x" ]
             , EFun ([ "y" ], EList [ EIdentifier "x"; EIdentifier "y" ]) ) )
;;

let%test _ =
  parse_string ~consume:Prefix parse_declaration "let g x y z = (x, x, y, y, z, z)"
  = Result.ok
    @@ EDeclaration
         ( "g"
         , [ "x"; "y"; "z" ]
         , ETuple
             [ EIdentifier "x"
             ; EIdentifier "x"
             ; EIdentifier "y"
             ; EIdentifier "y"
             ; EIdentifier "z"
             ; EIdentifier "z"
             ] )
;;

(* Tests for conditionals parser *)
let%test _ =
  parse_string
    ~consume:Prefix
    parse_expression
    "  if (true) then (1, 2) else (fun _ -> [1; 2; 3])"
  = Result.ok
    @@ EIf
         ( ELiteral (LBool true)
         , ETuple [ ELiteral (LInt 1); ELiteral (LInt 2) ]
         , EFun
             ([ "_" ], EList [ ELiteral (LInt 1); ELiteral (LInt 2); ELiteral (LInt 3) ])
         )
;;

(* Tests for pattern matching *)
let%test _ =
  parse_string ~consume:Prefix parse_expression " match x with _ -> false"
  = Result.ok @@ EMatchWith (EIdentifier "x", [ EIdentifier "_", ELiteral (LBool false) ])
;;

let%test _ =
  parse_string
    ~consume:Prefix
    parse_expression
    " match [x; y] with\n  | [1; 5] -> 12\n  | [5; 1] -> 12\n  | _ -> 0"
  = Result.ok
    @@ EMatchWith
         ( EList [ EIdentifier "x"; EIdentifier "y" ]
         , [ EList [ ELiteral (LInt 1); ELiteral (LInt 5) ], ELiteral (LInt 12)
           ; EList [ ELiteral (LInt 5); ELiteral (LInt 1) ], ELiteral (LInt 12)
           ; EIdentifier "_", ELiteral (LInt 0)
           ] )
;;

let%test _ =
  parse_string
    ~consume:Prefix
    parse_expression
    " match x, y, z with\n\
    \  | true, true, false -> true\n\
    \  | true, false, true -> true\n\
    \  | false, true, true -> true\n\
    \  | _ -> false"
  = Result.ok
    @@ EMatchWith
         ( ETuple [ EIdentifier "x"; EIdentifier "y"; EIdentifier "z" ]
         , [ ( ETuple
                 [ ELiteral (LBool true); ELiteral (LBool true); ELiteral (LBool false) ]
             , ELiteral (LBool true) )
           ; ( ETuple
                 [ ELiteral (LBool true); ELiteral (LBool false); ELiteral (LBool true) ]
             , ELiteral (LBool true) )
           ; ( ETuple
                 [ ELiteral (LBool false); ELiteral (LBool true); ELiteral (LBool true) ]
             , ELiteral (LBool true) )
           ; EIdentifier "_", ELiteral (LBool false)
           ] )
;;

(* Tests for arithmetic expressions  *)
let%test _ =
  parse_string ~consume:Prefix parse_expression "1 + 2"
  = Result.ok @@ EBinaryOperation (Add, ELiteral (LInt 1), ELiteral (LInt 2))
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression "1 + 2 * 3"
  = Result.ok
    @@ EBinaryOperation
         ( Add
         , ELiteral (LInt 1)
         , EBinaryOperation (Mul, ELiteral (LInt 2), ELiteral (LInt 3)) )
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression "1 + 2 * 3 == 7"
  = Result.ok
    @@ EBinaryOperation
         ( Eq
         , EBinaryOperation
             ( Add
             , ELiteral (LInt 1)
             , EBinaryOperation (Mul, ELiteral (LInt 2), ELiteral (LInt 3)) )
         , ELiteral (LInt 7) )
;;

let%test _ =
  parse_string
    ~consume:Prefix
    parse_expression
    "1 <= 3 && 2 <= 4 && true && 3 > 1 || 1 == 7"
  = Result.ok
    @@ EBinaryOperation
         ( OR
         , EBinaryOperation
             ( AND
             , EBinaryOperation
                 ( AND
                 , EBinaryOperation
                     ( AND
                     , EBinaryOperation (LTE, ELiteral (LInt 1), ELiteral (LInt 3))
                     , EBinaryOperation (LTE, ELiteral (LInt 2), ELiteral (LInt 4)) )
                 , ELiteral (LBool true) )
             , EBinaryOperation (GT, ELiteral (LInt 3), ELiteral (LInt 1)) )
         , EBinaryOperation (Eq, ELiteral (LInt 1), ELiteral (LInt 7)) )
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression "1 + (2 + 3)"
  = Result.ok
    @@ EBinaryOperation
         ( Add
         , ELiteral (LInt 1)
         , EBinaryOperation (Add, ELiteral (LInt 2), ELiteral (LInt 3)) )
;;

let%test _ =
  parse_string ~consume:Prefix parse_declaration "let f x y = x + y"
  = Result.ok
    @@ EDeclaration
         ("f", [ "x"; "y" ], EBinaryOperation (Add, EIdentifier "x", EIdentifier "y"))
;;

let%test _ =
  parse_string
    ~consume:Prefix
    parse_expression
    "if x && y / z then (x + y) / z else (x + y) * z"
  = Result.ok
    @@ EIf
         ( EBinaryOperation
             ( AND
             , EIdentifier "x"
             , EBinaryOperation (Div, EIdentifier "y", EIdentifier "z") )
         , EBinaryOperation
             ( Div
             , EBinaryOperation (Add, EIdentifier "x", EIdentifier "y")
             , EIdentifier "z" )
         , EBinaryOperation
             ( Mul
             , EBinaryOperation (Add, EIdentifier "x", EIdentifier "y")
             , EIdentifier "z" ) )
;;

let parse = Error "TODO"