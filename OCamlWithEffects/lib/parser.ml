open Angstrom
open Ast
open List
open String

type error_message = string
type input = string

type dispatch =
  { parse_list_constructing : dispatch -> expression Angstrom.t
  ; parse_tuple : dispatch -> expression Angstrom.t
  ; parse_unary_operation : dispatch -> expression Angstrom.t
  ; parse_list : dispatch -> expression Angstrom.t
  ; parse_application : dispatch -> expression Angstrom.t
  ; parse_fun : dispatch -> expression Angstrom.t
  ; parse_conditional : dispatch -> expression Angstrom.t
  ; parse_matching : dispatch -> expression Angstrom.t
  ; parse_binary_operation : dispatch -> expression Angstrom.t
  ; parse_let_in : dispatch -> expression Angstrom.t
  ; parse_data_constructor : dispatch -> expression Angstrom.t
  ; parse_expression : dispatch -> expression Angstrom.t
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
(* -------------------------------------- *)

(* Smart constructors for binary operators *)
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
(* --------------------------------------- *)

(* Smart constructors for unary operators *)
let uminus _ = Minus
let unot _ = Not
(* -------------------------------------- *)

(* Helpers *)
let space_predicate x = x == ' ' || x == '\n' || x == '\t' || x == '\r'
let remove_spaces = take_while space_predicate
let parens parser = remove_spaces *> char '(' *> parser <* remove_spaces <* char ')'

let parse_entity =
  fix
  @@ fun self ->
  remove_spaces
  *> (parens self
     <|> take_while1 (fun x ->
           contains "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'_" x)
     )
;;

let data_constructors = [ "Ok"; "Error"; "Some"; "None" ]
let keywords = [ "let"; "rec"; "match"; "with"; "if"; "then"; "else"; "in"; "fun"; "and" ]
(* ------- *)

(* Parsers *)
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

let parse_identifier =
  fix
  @@ fun _ ->
  remove_spaces
  *>
  let parse_identifier =
    parse_entity
    >>= fun entity ->
    if List.exists (( = ) entity) keywords || List.exists (( = ) entity) data_constructors
    then fail "Parsing error: keyword used."
    else return @@ eidentifier entity
  in
  (* many parse_literal
  >>= function
  (* TODO: true123 *)
  | [] -> parse_identifier
  | _ -> fail "Parsing error: not an identifier." *)
  parse_identifier
;;

let parse_tuple d =
  fix
  @@ fun self ->
  remove_spaces
  *> ((let parse_content =
         choice
           [ d.parse_list_constructing d
           ; parens self
           ; d.parse_unary_operation d
           ; d.parse_list d
           ; d.parse_application d
           ; d.parse_fun d
           ; d.parse_conditional d
           ; d.parse_matching d
           ; d.parse_binary_operation d
           ; d.parse_let_in d
           ; d.parse_data_constructor d
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
      [ d.parse_list_constructing d
      ; d.parse_tuple d
      ; d.parse_unary_operation d
      ; self
      ; d.parse_application d
      ; d.parse_fun d
      ; d.parse_conditional d
      ; d.parse_matching d
      ; d.parse_binary_operation d
      ; d.parse_let_in d
      ; d.parse_data_constructor d
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
      [ d.parse_list_constructing d
      ; d.parse_tuple d
      ; d.parse_unary_operation d
      ; d.parse_list d
      ; d.parse_application d
      ; self
      ; d.parse_conditional d
      ; d.parse_matching d
      ; d.parse_binary_operation d
      ; d.parse_let_in d
      ; d.parse_data_constructor d
      ; parse_literal
      ; parse_identifier
      ]
  in
  parens self
  <|> string "fun"
      *> lift2
           efun
           (many1 parse_entity <* remove_spaces <* string "->" <* remove_spaces)
           (parse_content <* remove_spaces)
;;

(* Used in parse_declaration and parse_let_in *)
let declaration_helper constructing_function d =
  let parse_content =
    choice
      [ d.parse_list_constructing d
      ; d.parse_tuple d
      ; d.parse_unary_operation d
      ; d.parse_list d
      ; d.parse_application d
      ; d.parse_fun d
      ; d.parse_conditional d
      ; d.parse_matching d
      ; d.parse_binary_operation d
      ; d.parse_let_in d
      ; d.parse_data_constructor d
      ; parse_literal
      ; parse_identifier
      ]
  in
  lift3
    constructing_function
    parse_entity
    (many parse_entity)
    (remove_spaces *> string "=" *> parse_content)
;;

let parse_declaration d =
  fix
  @@ fun _ ->
  remove_spaces *> string "let" *> remove_spaces *> (many @@ string "rec")
  >>= fun parsed_rec ->
  match parsed_rec with
  | [] -> declaration_helper edeclaration d
  | [ _ ] -> declaration_helper erecursivedeclaration d
  | _ -> fail "Parsing error: too many \"rec\"."
;;

let parse_let_in d =
  fix
  @@ fun self ->
  remove_spaces *> string "let" *> remove_spaces *> (many @@ string "rec")
  >>= fun parsed_rec ->
  let parse_content =
    choice
      [ d.parse_list_constructing d
      ; d.parse_tuple d
      ; d.parse_unary_operation d
      ; d.parse_list d
      ; d.parse_application d
      ; d.parse_fun d
      ; d.parse_conditional d
      ; d.parse_matching d
      ; d.parse_binary_operation d
      ; self
      ; d.parse_data_constructor d
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

let parse_conditional d =
  fix
  @@ fun self ->
  remove_spaces
  *>
  let parse_content =
    choice
      [ d.parse_list_constructing d
      ; d.parse_tuple d
      ; d.parse_unary_operation d
      ; d.parse_list d
      ; d.parse_application d
      ; d.parse_fun d
      ; self
      ; d.parse_matching d
      ; d.parse_binary_operation d
      ; d.parse_let_in d
      ; d.parse_data_constructor d
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
      [ d.parse_list_constructing d
      ; d.parse_tuple d
      ; d.parse_unary_operation d
      ; d.parse_list d
      ; d.parse_application d
      ; d.parse_fun d
      ; d.parse_conditional d
      ; self
      ; d.parse_binary_operation d
      ; d.parse_let_in d
      ; d.parse_data_constructor d
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
    remove_spaces *> choice [ string "=" >>| beq; string "!=" <|> string "<>" >>| bneq ]
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
      [ d.parse_unary_operation d
      ; d.parse_application d
      ; d.parse_conditional d
      ; d.parse_matching d
      ; parens self
      ; d.parse_let_in d
      ; d.parse_data_constructor d
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
  >>= fun result ->
  match result with
  | EBinaryOperation (_, _, _) -> return result
  | _ -> fail "Parsing error: not binary operation."
;;

let parse_application d =
  fix
  @@ fun self ->
  remove_spaces
  *> (parens self
     <|>
     let function_parser =
       choice
         [ parens @@ d.parse_fun d
         ; parens @@ d.parse_conditional d
         ; parens @@ d.parse_matching d
         ; parens @@ d.parse_let_in d
         ; parse_identifier
         ]
     and operand_parser =
       choice
         [ parens @@ d.parse_list_constructing d
         ; d.parse_tuple d
         ; parens @@ d.parse_unary_operation d
         ; d.parse_list d
         ; parens self
         ; d.parse_fun d
         ; d.parse_conditional d
         ; d.parse_matching d
         ; d.parse_binary_operation d
         ; d.parse_let_in d
         ; d.parse_data_constructor d
         ; parse_literal
         ; parse_identifier
         ]
     in
     let apply_lift acc = lift (eapplication acc) operand_parser in
     let rec go acc = apply_lift acc >>= go <|> return acc in
     function_parser >>= fun init -> apply_lift init >>= fun init -> go init)
;;

let parse_data_constructor d =
  fix
  @@ fun self ->
  remove_spaces
  *>
  let parse_content =
    choice
      [ parens @@ d.parse_list_constructing d
      ; parens @@ d.parse_tuple d
      ; parens @@ d.parse_unary_operation d
      ; d.parse_list d
      ; parens @@ d.parse_application d
      ; parens @@ d.parse_fun d
      ; parens @@ d.parse_conditional d
      ; parens @@ d.parse_matching d
      ; parens @@ d.parse_binary_operation d
      ; parens @@ d.parse_let_in d
      ; parens self
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
  let parse_content_minus =
    let indent = many1 (satisfy space_predicate) in
    choice
      [ parens self <|> indent *> self
      ; parens @@ d.parse_application d <|> indent *> d.parse_application d
      ; parens @@ d.parse_conditional d <|> indent *> d.parse_conditional d
      ; parens @@ d.parse_matching d <|> indent *> d.parse_matching d
      ; parens @@ d.parse_let_in d <|> indent *> d.parse_let_in d
      ; parse_literal
      ; parse_identifier
      ; parens @@ d.parse_binary_operation d
      ]
  and parse_content_not =
    choice
      [ parens self
      ; parens @@ d.parse_application d
      ; parens @@ d.parse_conditional d
      ; parens @@ d.parse_matching d
      ; parens @@ d.parse_binary_operation d
      ; parens @@ d.parse_let_in d
      ; parse_literal
      ; parse_identifier
      ]
  in
  parens self
  <|> lift2 eunary_operation (char '-' >>| uminus) parse_content_minus
  <|> lift2 eunary_operation (string "not" >>| unot) parse_content_not
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
         [ parens self
         ; parens @@ d.parse_tuple d
         ; d.parse_unary_operation d
         ; d.parse_list d
         ; d.parse_application d
         ; parens @@ d.parse_fun d
         ; parens @@ d.parse_conditional d
         ; parens @@ d.parse_matching d
         ; parens @@ d.parse_binary_operation d
         ; d.parse_let_in d
         ; d.parse_data_constructor d
         ; parse_literal
         ; parse_identifier
         ]
     in
     lift2 econstruct_list (parse_content <* separator) (self <|> parse_content))
;;

(* ------- *)

let parse_expression d =
  choice
    [ d.parse_list_constructing d
    ; d.parse_tuple d
    ; d.parse_unary_operation d
    ; d.parse_list d
    ; d.parse_application d
    ; d.parse_fun d
    ; d.parse_conditional d
    ; d.parse_matching d
    ; d.parse_binary_operation d
    ; d.parse_let_in d
    ; d.parse_data_constructor d
    ; parse_literal
    ; parse_identifier
    ]
;;

let default =
  { parse_list_constructing
  ; parse_tuple
  ; parse_unary_operation
  ; parse_list
  ; parse_application
  ; parse_fun
  ; parse_conditional
  ; parse_matching
  ; parse_binary_operation
  ; parse_let_in
  ; parse_data_constructor
  ; parse_expression
  }
;;

let parse_tuple = parse_tuple default
let parse_list = parse_list default
let parse_fun = parse_fun default
let parse_declaration = parse_declaration default
let parse_conditional = parse_conditional default
let parse_matching = parse_matching default
let parse_binary_operation = parse_binary_operation default
let parse_let_in = parse_let_in default
let parse_application = parse_application default
let parse_unary_operation = parse_unary_operation default
let parse_list_constructing = parse_list_constructing default
let parse_data_constructor = parse_data_constructor default
let parse_expression = parse_expression default

(* Main parsing function *)
let parse : input -> (expression list, error_message) result =
 fun program ->
  parse_string ~consume:All (many parse_declaration <* remove_spaces) program
;;

(* -------------------- TESTS -------------------- *)

(* 1 *)
let%test _ =
  parse
    "let rec factorial n acc = if n <= 1 then acc else factorial (n - 1) (acc * n)\n\
     let main = factorial 5 1"
  = Result.ok
    @@ [ ERecursiveDeclaration
           ( "factorial"
           , [ "n"; "acc" ]
           , EIf
               ( EBinaryOperation (LTE, EIdentifier "n", ELiteral (LInt 1))
               , EIdentifier "acc"
               , EApplication
                   ( EApplication
                       ( EIdentifier "factorial"
                       , EBinaryOperation (Sub, EIdentifier "n", ELiteral (LInt 1)) )
                   , EBinaryOperation (Mul, EIdentifier "acc", EIdentifier "n") ) ) )
       ; EDeclaration
           ( "main"
           , []
           , EApplication
               ( EApplication (EIdentifier "factorial", ELiteral (LInt 5))
               , ELiteral (LInt 1) ) )
       ]
;;

(* 2 *)
let%test _ =
  parse " let main = 1 :: 2 :: [] "
  = Result.ok
    @@ [ EDeclaration
           ( "main"
           , []
           , EConstructList
               (ELiteral (LInt 1), EConstructList (ELiteral (LInt 2), EList [])) )
       ]
;;

(* 3 *)
let%test _ =
  parse " let main = true :: (false) :: [false] "
  = Result.ok
    @@ [ EDeclaration
           ( "main"
           , []
           , EConstructList
               ( ELiteral (LBool true)
               , EConstructList (ELiteral (LBool false), EList [ ELiteral (LBool false) ])
               ) )
       ]
;;

(* 4 *)
let%test _ =
  parse " let main = (10 + 20) :: [30; 4 * 10] "
  = Result.ok
    @@ [ EDeclaration
           ( "main"
           , []
           , EConstructList
               ( EBinaryOperation (Add, ELiteral (LInt 10), ELiteral (LInt 20))
               , EList
                   [ ELiteral (LInt 30)
                   ; EBinaryOperation (Mul, ELiteral (LInt 4), ELiteral (LInt 10))
                   ] ) )
       ]
;;

(* 5 *)
let%test _ =
  parse " let main = (fun x -> 'a') :: [fun _ -> 'b'] "
  = Result.ok
    @@ [ EDeclaration
           ( "main"
           , []
           , EConstructList
               ( EFun ([ "x" ], ELiteral (LChar 'a'))
               , EList [ EFun ([ "_" ], ELiteral (LChar 'b')) ] ) )
       ]
;;

(* 6 *)
let%test _ =
  parse " let main = () :: (()) :: ((())) :: (((()))) :: [] "
  = Result.ok
    @@ [ EDeclaration
           ( "main"
           , []
           , EConstructList
               ( ELiteral LUnit
               , EConstructList
                   ( ELiteral LUnit
                   , EConstructList
                       (ELiteral LUnit, EConstructList (ELiteral LUnit, EList [])) ) ) )
       ]
;;

(* 7 *)
let%test _ =
  parse " let main = [\"apple\";\n\"orange\";\n\"banana\";\n\"pear\"] "
  = Result.ok
    @@ [ EDeclaration
           ( "main"
           , []
           , EList
               [ ELiteral (LString "apple")
               ; ELiteral (LString "orange")
               ; ELiteral (LString "banana")
               ; ELiteral (LString "pear")
               ] )
       ]
;;

(* 8 *)
let%test _ =
  parse " let main = [ 'h' ; 'e' ; 'l' ; 'l' ; 'o' ] "
  = Result.ok
    @@ [ EDeclaration
           ( "main"
           , []
           , EList
               [ ELiteral (LChar 'h')
               ; ELiteral (LChar 'e')
               ; ELiteral (LChar 'l')
               ; ELiteral (LChar 'l')
               ; ELiteral (LChar 'o')
               ] )
       ]
;;

(* 9 *)
let%test _ =
  parse " let main = [1] "
  = Result.ok @@ [ EDeclaration ("main", [], EList [ ELiteral (LInt 1) ]) ]
;;

(* 10 *)
let%test _ =
  parse " let main = [] " = Result.ok @@ [ EDeclaration ("main", [], EList []) ]
;;

(* 11 *)
let%test _ =
  parse
    " let main = [let x = 5 and y = 7 in x + y; (fun t -> t - 1) 10; if (5 >= 1) then 1 \
     else 0] "
  = Result.ok
    @@ [ EDeclaration
           ( "main"
           , []
           , EList
               [ ELetIn
                   ( [ EDeclaration ("x", [], ELiteral (LInt 5))
                     ; EDeclaration ("y", [], ELiteral (LInt 7))
                     ]
                   , EBinaryOperation (Add, EIdentifier "x", EIdentifier "y") )
               ; EApplication
                   ( EFun
                       ( [ "t" ]
                       , EBinaryOperation (Sub, EIdentifier "t", ELiteral (LInt 1)) )
                   , ELiteral (LInt 10) )
               ; EIf
                   ( EBinaryOperation (GTE, ELiteral (LInt 5), ELiteral (LInt 1))
                   , ELiteral (LInt 1)
                   , ELiteral (LInt 0) )
               ] )
       ]
;;

(* 12 *)
let%test _ =
  parse
    " let main = (if x > 0 then 1 else (if x = 0 then 0 else -1)) :: [0 ; (if y > 0 then \
     1 else (if y = 0 then 0 else -1)) ; 0] "
  = Result.ok
    @@ [ EDeclaration
           ( "main"
           , []
           , EConstructList
               ( EIf
                   ( EBinaryOperation (GT, EIdentifier "x", ELiteral (LInt 0))
                   , ELiteral (LInt 1)
                   , EIf
                       ( EBinaryOperation (Eq, EIdentifier "x", ELiteral (LInt 0))
                       , ELiteral (LInt 0)
                       , EUnaryOperation (Minus, ELiteral (LInt 1)) ) )
               , EList
                   [ ELiteral (LInt 0)
                   ; EIf
                       ( EBinaryOperation (GT, EIdentifier "y", ELiteral (LInt 0))
                       , ELiteral (LInt 1)
                       , EIf
                           ( EBinaryOperation (Eq, EIdentifier "y", ELiteral (LInt 0))
                           , ELiteral (LInt 0)
                           , EUnaryOperation (Minus, ELiteral (LInt 1)) ) )
                   ; ELiteral (LInt 0)
                   ] ) )
       ]
;;

(* 13 *)
let%test _ =
  parse " let main = fun x y z -> x + y * z "
  = Result.ok
    @@ [ EDeclaration
           ( "main"
           , []
           , EFun
               ( [ "x"; "y"; "z" ]
               , EBinaryOperation
                   ( Add
                   , EIdentifier "x"
                   , EBinaryOperation (Mul, EIdentifier "y", EIdentifier "z") ) ) )
       ]
;;

(* 14 *)
let%test _ =
  parse " let main = fun _ -> 42 "
  = Result.ok @@ [ EDeclaration ("main", [], EFun ([ "_" ], ELiteral (LInt 42))) ]
;;

(* 15 *)
let%test _ =
  parse " let main = fun _ -> fun _ -> \"Hello\" "
  = Result.ok
    @@ [ EDeclaration
           ("main", [], EFun ([ "_" ], EFun ([ "_" ], ELiteral (LString "Hello"))))
       ]
;;

(* 16 *)
let%test _ =
  parse " let main = fun x y -> if x < 0 then [x;y] else [0;y] "
  = Result.ok
    @@ [ EDeclaration
           ( "main"
           , []
           , EFun
               ( [ "x"; "y" ]
               , EIf
                   ( EBinaryOperation (LT, EIdentifier "x", ELiteral (LInt 0))
                   , EList [ EIdentifier "x"; EIdentifier "y" ]
                   , EList [ ELiteral (LInt 0); EIdentifier "y" ] ) ) )
       ]
;;

(* 17 *)
let%test _ =
  parse " let main = (-2, -3) :: [-5, -10; -6, -12; 7, -8] "
  = Result.ok
    @@ [ EDeclaration
           ( "main"
           , []
           , EConstructList
               ( ETuple
                   [ EUnaryOperation (Minus, ELiteral (LInt 2))
                   ; EUnaryOperation (Minus, ELiteral (LInt 3))
                   ]
               , EList
                   [ ETuple
                       [ EUnaryOperation (Minus, ELiteral (LInt 5))
                       ; EUnaryOperation (Minus, ELiteral (LInt 10))
                       ]
                   ; ETuple
                       [ EUnaryOperation (Minus, ELiteral (LInt 6))
                       ; EUnaryOperation (Minus, ELiteral (LInt 12))
                       ]
                   ; ETuple
                       [ ELiteral (LInt 7); EUnaryOperation (Minus, ELiteral (LInt 8)) ]
                   ] ) )
       ]
;;

(* 18 *)
let%test _ =
  parse " let main = \"Danya\", \"Ilya\" "
  = Result.ok
    @@ [ EDeclaration
           ("main", [], ETuple [ ELiteral (LString "Danya"); ELiteral (LString "Ilya") ])
       ]
;;

(* 19 *)
let%test _ =
  parse " let main = ( 123\t, \"aaa\"\t, 'b'\n, true\t, ()\t ) "
  = Result.ok
    @@ [ EDeclaration
           ( "main"
           , []
           , ETuple
               [ ELiteral (LInt 123)
               ; ELiteral (LString "aaa")
               ; ELiteral (LChar 'b')
               ; ELiteral (LBool true)
               ; ELiteral LUnit
               ] )
       ]
;;

(* 20 *)
let%test _ =
  parse " let main = (fun _ -> 1, fun _ -> 2) "
  = Result.ok
    @@ [ EDeclaration
           ( "main"
           , []
           , EFun
               ([ "_" ], ETuple [ ELiteral (LInt 1); EFun ([ "_" ], ELiteral (LInt 2)) ])
           )
       ]
;;

(* 21 *)
let%test _ =
  parse " let main = [fun _ -> 1; fun _ -> 2] "
  = Result.ok
    @@ [ EDeclaration
           ( "main"
           , []
           , EList
               [ EFun ([ "_" ], ELiteral (LInt 1)); EFun ([ "_" ], ELiteral (LInt 2)) ] )
       ]
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
  parse_string ~consume:Prefix parse_expression "1 + 2 * 3 = 7"
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
    "1 <= 3 && 2 <= 4 && true && 3 > 1 || 1 = 7"
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
  parse_string ~consume:Prefix parse_expression "- ((fun x y -> (x + y)) 1 2)"
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
  parse_string ~consume:Prefix parse_expression " ( 1 + 2 + 3 ) "
  = Result.ok
    @@ EBinaryOperation
         ( Add
         , EBinaryOperation (Add, ELiteral (LInt 1), ELiteral (LInt 2))
         , ELiteral (LInt 3) )
;;

let%test _ =
  parse_string ~consume:Prefix parse_expression " if not x then -1 else 0 "
  = Result.ok
    @@ EIf
         ( EUnaryOperation (Not, EIdentifier "x")
         , EUnaryOperation (Minus, ELiteral (LInt 1))
         , ELiteral (LInt 0) )
;;

(*Tests for list construction *)
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
;;