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
  ; parse_conditional : dispatch -> expression Angstrom.t
  ; parse_matching : dispatch -> expression Angstrom.t
  ; parse_binary_operation : dispatch -> expression Angstrom.t
  ; parse_let_in : dispatch -> expression Angstrom.t
  ; parse_application : dispatch -> expression Angstrom.t
  ; parse_data_constructor : dispatch -> expression Angstrom.t
  ; parse_unary_operation : dispatch -> expression Angstrom.t
  ; parse_list_constructing : dispatch -> expression Angstrom.t
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

let erecursivedeclaration function_name variable_list expression =
  ERecursiveDeclaration (function_name, variable_list, expression)
;;

let eif condition true_branch false_branch = EIf (condition, true_branch, false_branch)
let ematchwith expression cases = EMatchWith (expression, cases)
let eletin declaration_list body = ELetIn (declaration_list, body)

let eapplication function_expression operand_expression =
  EApplication (function_expression, operand_expression)
;;

let edata_constructor constructor_name expressions =
  EDataConstructor (constructor_name, expressions)
;;

let eunary_operation operation expression = EUnaryOperation (operation, expression)
let econstruct_list head tail = EConstructList (head, tail)

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

(* Unary operator smart constructors *)
let uminus _ = Minus
let unot _ = Not
let space_predicate x = x == ' ' || x == '\n' || x == '\t' || x == '\r'
let remove_spaces = take_while space_predicate

let%test _ =
  parse_string ~consume:Prefix (remove_spaces *> many any_char) "   \t\n\r   s"
  = Result.ok @@ [ 's' ]
;;

let parens parser = remove_spaces *> char '(' *> parser <* remove_spaces <* char ')'

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

let data_constructors = [ "Ok"; "Error"; "Some"; "None" ]
let keywords = [ "let"; "rec"; "match"; "with"; "if"; "then"; "else"; "in"; "fun" ]

let parse_identifier =
  parse_entity
  >>= fun entity ->
  if List.exists (( = ) entity) keywords || List.exists (( = ) entity) data_constructors
  then fail "Parsing error: keyword used."
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
  ; d.parse_data_constructor d
  ; parse_identifier
  ; d.parse_let_in d
  ]
;;

(* -------------------------------------- *)

let parse_tuple d =
  fix
  @@ fun self ->
  remove_spaces
  *> ((let parse_content =
         choice
           [ d.parse_list d
           ; parens @@ d.parse_fun d
           ; parens @@ d.parse_conditional d
           ; parens @@ d.parse_matching d
           ; d.parse_binary_operation d
           ; parens @@ d.parse_let_in d
           ; d.parse_application d
           ; d.parse_data_constructor d
           ; d.parse_unary_operation d
           ; d.parse_list_constructing d
           ; parse_literal
           ; parse_identifier
           ]
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
  and separator = remove_spaces *> char ';' *> remove_spaces <|> remove_spaces
  and parse_content =
    choice
      [ d.parse_tuple d
      ; d.parse_fun d
      ; d.parse_conditional d
      ; d.parse_matching d
      ; d.parse_binary_operation d
      ; d.parse_let_in d
      ; d.parse_application d
      ; d.parse_data_constructor d
      ; d.parse_unary_operation d
      ; d.parse_list_constructing d
      ; parse_literal
      ; parse_identifier
      ]
  in
  parens self
  <|> lift elist @@ brackets @@ (remove_spaces *> many (parse_content <* separator))
;;

let parse_fun d =
  fix
  @@ fun self ->
  remove_spaces
  *>
  let parse_content =
    choice
      [ d.parse_tuple d
      ; d.parse_list d
      ; d.parse_conditional d
      ; d.parse_matching d
      ; d.parse_binary_operation d
      ; d.parse_let_in d
      ; d.parse_application d
      ; d.parse_data_constructor d
      ; d.parse_unary_operation d
      ; d.parse_list_constructing d
      ; parse_literal
      ; parse_identifier
      ]
  in
  parens self
  <|> string "fun"
      *> lift2
           efun
           (many1 (parse_entity >>= fun v -> if v = "->" then fail "" else return v)
           <* remove_spaces
           <* string "->"
           <* remove_spaces)
           (parse_content <* remove_spaces)
;;

(* Declaration parsing*)
let declaration_helper constructing_function d =
  let parse_content =
    choice
      [ d.parse_tuple d
      ; d.parse_list d
      ; d.parse_fun d
      ; d.parse_conditional d
      ; d.parse_matching d
      ; d.parse_binary_operation d
      ; d.parse_let_in d
      ; d.parse_application d
      ; d.parse_data_constructor d
      ; d.parse_unary_operation d
      ; d.parse_list_constructing d
      ; parse_literal
      ; parse_identifier
      ]
  in
  lift3
    constructing_function
    parse_entity
    (many (parse_entity >>= fun v -> if v = "=" then fail "" else return v))
    (remove_spaces *> string "=" *> parse_content)
;;

let parse_declaration d =
  remove_spaces *> string "let" *> remove_spaces *> (many @@ string "rec")
  >>= fun parsed_rec ->
  match parsed_rec with
  | [] -> declaration_helper edeclaration d
  | [ _ ] -> declaration_helper erecursivedeclaration d
  | _ -> fail "Parsing error: too many \"rec\"."
;;

let parse_let_in d =
  remove_spaces *> string "let" *> remove_spaces *> (many @@ string "rec")
  >>= fun parsed_rec ->
  let parse_content =
    choice
      [ d.parse_tuple d
      ; d.parse_list d
      ; d.parse_fun d
      ; d.parse_conditional d
      ; d.parse_matching d
      ; d.parse_binary_operation d
      ; d.parse_let_in d
      ; d.parse_application d
      ; d.parse_data_constructor d
      ; d.parse_unary_operation d
      ; d.parse_list_constructing d
      ; parse_literal
      ; parse_identifier
      ]
  in
  lift2
    eletin
    (let separator = remove_spaces *> string "and" *> remove_spaces in
     match parsed_rec with
     | [] -> sep_by1 separator @@ declaration_helper edeclaration d
     | [ _ ] -> sep_by1 separator @@ declaration_helper erecursivedeclaration d
     | _ -> fail "Parsing error: too many \"rec\".")
    (remove_spaces *> string "in" *> parse_content)
;;

(* -------------------*)

let parse_conditional d =
  fix
  @@ fun self ->
  remove_spaces
  *>
  let parse_content =
    choice
      [ d.parse_tuple d
      ; d.parse_list d
      ; d.parse_fun d
      ; d.parse_matching d
      ; d.parse_binary_operation d
      ; d.parse_let_in d
      ; d.parse_application d
      ; d.parse_data_constructor d
      ; d.parse_unary_operation d
      ; d.parse_list_constructing d
      ; parse_literal
      ; parse_identifier
      ]
  in
  parens self
  <|> string "if"
      *> lift3
           eif
           parse_content
           (remove_spaces *> string "then" *> parse_content)
           (remove_spaces *> string "else" *> parse_content)
;;

let parse_matching d =
  fix
  @@ fun self ->
  remove_spaces
  *>
  let parse_content =
    choice
      [ d.parse_tuple d
      ; d.parse_list d
      ; d.parse_fun d
      ; d.parse_conditional d
      ; d.parse_binary_operation d
      ; d.parse_let_in d
      ; d.parse_application d
      ; d.parse_data_constructor d
      ; d.parse_unary_operation d
      ; d.parse_list_constructing d
      ; parse_literal
      ; parse_identifier
      ]
  in
  parens self
  <|> string "match"
      *> lift2
           ematchwith
           parse_content
           (let parse_case =
              lift2
                (fun case action -> case, action)
                parse_content
                (remove_spaces *> string "->" *> parse_content)
            and separator = remove_spaces *> string "|" in
            remove_spaces
            *> string "with"
            *> remove_spaces
            *> (string "|" <|> remove_spaces)
            *> sep_by1 separator parse_case)
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
  let parse_content =
    choice
      [ d.parse_tuple d
      ; d.parse_list d
      ; d.parse_fun d
      ; d.parse_conditional d
      ; d.parse_matching d
      ; parens @@ self
      ; d.parse_let_in d
      ; d.parse_application d
      ; d.parse_data_constructor d
      ; d.parse_unary_operation d
      ; d.parse_list_constructing d
      ; parse_literal
      ; parse_identifier
      ]
  in
  parse_content
  <||> multiplicative
  <||> additive
  <||> relational
  <||> equality
  <||> logical_and
  <||> logical_or
  >>= function
  | EBinaryOperation (operator, left_operand, right_operand) ->
    return @@ ebinary_operation operator left_operand right_operand
  | _ -> fail "Parsing error: not binary operation."
;;

let parse_application d =
  fix
  @@ fun self ->
  remove_spaces
  *> (parens self
     <|>
     let operand_parser =
       choice
         [ d.parse_tuple d
         ; d.parse_list d
         ; d.parse_fun d
         ; d.parse_conditional d
         ; d.parse_matching d
         ; d.parse_binary_operation d
         ; d.parse_let_in d
         ; d.parse_data_constructor d
         ; d.parse_unary_operation d
         ; d.parse_list_constructing d
         ; parse_literal
         ; parse_identifier
         ]
     in
     let apply_lift acc = lift (eapplication acc) operand_parser in
     let rec go acc = apply_lift acc >>= go <|> return acc in
     parse_identifier
     <|> parens @@ d.parse_fun d
     >>= fun init -> apply_lift init >>= fun init -> go init)
;;

let parse_data_constructor d =
  fix
  @@ fun self ->
  remove_spaces
  *>
  let parse_content =
    choice
      [ d.parse_tuple d
      ; d.parse_list d
      ; d.parse_fun d
      ; d.parse_conditional d
      ; d.parse_matching d
      ; d.parse_binary_operation d
      ; d.parse_let_in d
      ; d.parse_application d
      ; d.parse_unary_operation d
      ; d.parse_list_constructing d
      ; parse_literal
      ; parse_identifier
      ]
  in
  parens self
  <|> (lift2
         (fun constructor_name expression_list -> constructor_name, expression_list)
         parse_entity
         (many parse_content)
      >>= function
      | constructor_name, expression_list ->
        if List.exists (( = ) constructor_name) data_constructors
        then return @@ edata_constructor constructor_name expression_list
        else fail "Parsing error: invalid constructor.")
;;

let parse_unary_operation d =
  fix
  @@ fun self ->
  remove_spaces
  *>
  let parse_content =
    choice
      [ d.parse_tuple d
      ; d.parse_list d
      ; d.parse_fun d
      ; d.parse_conditional d
      ; d.parse_matching d
      ; d.parse_binary_operation d
      ; d.parse_let_in d
      ; d.parse_application d
      ; d.parse_data_constructor d
      ; d.parse_list_constructing d
      ; parse_literal
      ; parse_identifier
      ]
  in
  parens self
  <|>
  let parse_unary_operator = choice [ char '-' >>| uminus; char '!' >>| unot ] in
  lift2 eunary_operation parse_unary_operator parse_content
;;

let parse_expression d =
  choice
    [ d.parse_tuple d
    ; d.parse_list d
    ; d.parse_fun d
    ; d.parse_conditional d
    ; d.parse_matching d
    ; d.parse_binary_operation d
    ; d.parse_let_in d
    ; d.parse_application d
    ; d.parse_data_constructor d
    ; d.parse_unary_operation d
    ; d.parse_list_constructing d
    ; parse_literal
    ; parse_identifier
    ]
;;

let parse_list_constructing d =
  fix
  @@ fun self ->
  remove_spaces
  *> (parens self
     <|>
     let separator = remove_spaces *> string "::" *> remove_spaces
     and parse_content =
       choice
         [ d.parse_tuple d
         ; d.parse_list d
         ; d.parse_fun d
         ; d.parse_conditional d
         ; d.parse_matching d
         ; d.parse_binary_operation d
         ; d.parse_let_in d
         ; d.parse_application d
         ; d.parse_data_constructor d
         ; d.parse_unary_operation d
         ; parse_literal
         ; parse_identifier
         ]
     in
     lift2 econstruct_list (parse_content <* separator) (self <|> parse_content))
;;

let default =
  { parse_tuple
  ; parse_list
  ; parse_fun
  ; parse_expression
  ; parse_conditional
  ; parse_matching
  ; parse_binary_operation
  ; parse_let_in
  ; parse_application
  ; parse_data_constructor
  ; parse_unary_operation
  ; parse_list_constructing
  }
;;

let parse_tuple = default.parse_tuple default
let parse_list = default.parse_list default
let parse_fun = default.parse_fun default
let parse_expression = default.parse_expression default
let parse_declaration = parse_declaration default
let parse_conditional = default.parse_conditional default
let parse_matching = default.parse_matching default
let parse_binary_operation = default.parse_binary_operation default
let parse_let_in = default.parse_let_in default
let parse_application = default.parse_application default
let parse_unary_operation = default.parse_unary_operation default
let parse_list_constructing = default.parse_list_constructing default

(* 
(* Tests for list parsing *)
*)
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

(*

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
  parse_string ~consume:Prefix parse_expression " [ 1; 2; 3] "
  = Result.ok @@ EList [ ELiteral (LInt 1); ELiteral (LInt 2); ELiteral (LInt 3) ]
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression "  [ 1; [true; false]; (())]  "
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
  parse_string ~consume:Prefix parse_expression "((fun _ -> 5   ))"
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

let%test _ =
  parse_string ~consume:Prefix parse_declaration "let comma = ','"
  = Result.ok @@ EDeclaration ("comma", [], ELiteral (LChar ','))
;;

let%test _ =
  parse_string
    ~consume:Prefix
    parse_declaration
    "let rec helper acc n =\n match n with\n | 0 -> acc\n | _ -> helper (acc + n) (n - 1)"
  = Result.ok
    @@ ERecursiveDeclaration
         ( "helper"
         , [ "acc"; "n" ]
         , EMatchWith
             ( EIdentifier "n"
             , [ ELiteral (LInt 0), EIdentifier "acc"
               ; ( EIdentifier "_"
                 , EApplication
                     ( EApplication
                         ( EIdentifier "helper"
                         , EBinaryOperation (Add, EIdentifier "acc", EIdentifier "n") )
                     , EBinaryOperation (Sub, EIdentifier "n", ELiteral (LInt 1)) ) )
               ] ) )
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

(* Tests for let in *)
let%test _ =
  parse_string
    ~consume:Prefix
    parse_declaration
    " let triple_by_triple x = let triple = (x, x, x) in [ triple; triple; triple] "
  = Result.ok
    @@ EDeclaration
         ( "triple_by_triple"
         , [ "x" ]
         , ELetIn
             ( [ EDeclaration
                   ( "triple"
                   , []
                   , ETuple [ EIdentifier "x"; EIdentifier "x"; EIdentifier "x" ] )
               ]
             , EList [ EIdentifier "triple"; EIdentifier "triple"; EIdentifier "triple" ]
             ) )
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

let%test _ =
  parse_string
    ~consume:Prefix
    parse_declaration
    " let triple_by_triple x = let triple = (x, x, x) and double = (x, x) in [ triple; \
     double; triple] "
  = Result.ok
    @@ EDeclaration
         ( "triple_by_triple"
         , [ "x" ]
         , ELetIn
             ( [ EDeclaration
                   ( "triple"
                   , []
                   , ETuple [ EIdentifier "x"; EIdentifier "x"; EIdentifier "x" ] )
               ; EDeclaration ("double", [], ETuple [ EIdentifier "x"; EIdentifier "x" ])
               ]
             , EList [ EIdentifier "triple"; EIdentifier "double"; EIdentifier "triple" ]
             ) )
;;

(* Tests for application *)
let%test _ =
  parse_string ~consume:Prefix parse_expression " f x "
  = Result.ok @@ EApplication (EIdentifier "f", EIdentifier "x")
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression " f x y "
  = Result.ok
    @@ EApplication (EApplication (EIdentifier "f", EIdentifier "x"), EIdentifier "y")
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression " f [1; 2] n "
  = Result.ok
    @@ EApplication
         ( EApplication (EIdentifier "f", EList [ ELiteral (LInt 1); ELiteral (LInt 2) ])
         , EIdentifier "n" )
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression "(fun x y -> [x; y]) n 0"
  = Result.ok
    @@ EApplication
         ( EApplication
             ( EFun ([ "x"; "y" ], EList [ EIdentifier "x"; EIdentifier "y" ])
             , EIdentifier "n" )
         , ELiteral (LInt 0) )
;;

(* Tests for data constructor parser *)
let%test _ =
  parse_string ~consume:Prefix parse_expression "Ok 1"
  = Result.ok @@ EDataConstructor ("Ok", [ ELiteral (LInt 1) ])
;;

let%test _ =
  parse_string
    ~consume:Prefix
    parse_expression
    " match f x y with\n  | 100 -> x\n  | 200 -> y\n  | _ -> 0"
  = Result.ok
    @@ EMatchWith
         ( EApplication (EApplication (EIdentifier "f", EIdentifier "x"), EIdentifier "y")
         , [ ELiteral (LInt 100), EIdentifier "x"
           ; ELiteral (LInt 200), EIdentifier "y"
           ; EIdentifier "_", ELiteral (LInt 0)
           ] )
;;

let%test _ =
  parse_string
    ~consume:Prefix
    parse_expression
    "match f with Ok x -> x + 1 | _ -> let sq = fun y -> y * y in sq x"
  = Result.ok
    @@ EMatchWith
         ( EIdentifier "f"
         , [ ( EDataConstructor ("Ok", [ EIdentifier "x" ])
             , EBinaryOperation (Add, EIdentifier "x", ELiteral (LInt 1)) )
           ; ( EIdentifier "_"
             , ELetIn
                 ( [ EDeclaration
                       ( "sq"
                       , []
                       , EFun
                           ( [ "y" ]
                           , EBinaryOperation (Mul, EIdentifier "y", EIdentifier "y") ) )
                   ]
                 , EApplication (EIdentifier "sq", EIdentifier "x") ) )
           ] )
;;

(* Tests for unary operations parsing *)
let%test _ =
  parse_string ~consume:Prefix parse_expression "-2"
  = Result.ok @@ EUnaryOperation (Minus, ELiteral (LInt 2))
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression "-(x + y)"
  = Result.ok
    @@ EUnaryOperation (Minus, EBinaryOperation (Add, EIdentifier "x", EIdentifier "y"))
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression "- ((fun x y -> x + y) 1 2)"
  = Result.ok
    @@ EUnaryOperation
         ( Minus
         , EApplication
             ( EApplication
                 ( EFun
                     ( [ "x"; "y" ]
                     , EBinaryOperation (Add, EIdentifier "x", EIdentifier "y") )
                 , ELiteral (LInt 1) )
             , ELiteral (LInt 2) ) )
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression "if !x then -1 + x else !(x * (-x))"
  = Result.ok
    @@ EIf
         ( EUnaryOperation (Not, EIdentifier "x")
         , EBinaryOperation
             (Add, EUnaryOperation (Minus, ELiteral (LInt 1)), EIdentifier "x")
         , EUnaryOperation
             ( Not
             , EBinaryOperation
                 (Mul, EIdentifier "x", EUnaryOperation (Minus, EIdentifier "x")) ) )
;;

(* Tests for list construction *)

let%test _ =
  parse_string ~consume:Prefix parse_list_constructing " 1 :: [ 2; 3 ] "
  = Result.ok
    @@ EConstructList (ELiteral (LInt 1), EList [ ELiteral (LInt 2); ELiteral (LInt 3) ])
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression " (1 :: [ 2; 3 ], x :: [ 2; 3 ]) "
  = Result.ok
    @@ ETuple
         [ EConstructList
             (ELiteral (LInt 1), EList [ ELiteral (LInt 2); ELiteral (LInt 3) ])
         ; EConstructList (EIdentifier "x", EList [ ELiteral (LInt 2); ELiteral (LInt 3) ])
         ]
;; *)

let parse = Error "TODO"