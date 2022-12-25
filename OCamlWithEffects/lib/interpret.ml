open Ast
open Base
open List

module type MONAD_FAIL = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  val ( *> ) : ('a, 'e) t -> ('a, 'e) t -> ('a, 'e) t
end

type error_message = string

type rec_flag =
  | Recursive
  | NonRecursive

type environment =
  { ids : (id, value, Base.String.comparator_witness) Base.Map.t
  ; effects : (capitalized_id, Base.String.comparator_witness) Base.Set.t
  ; effect_handlers :
      (capitalized_id, expression, Base.String.comparator_witness) Base.Map.t
  }

and value =
  | VInt of int
  | VString of string
  | VBool of bool
  | VChar of char
  | VUnit
  | VList of value list
  | VTuple of value list
  | VFun of id list * expression * environment * rec_flag
  | VADT of data_constructor_name * value option
  | VEffectNoArg of id
  | VEffectArg of id * expression
  | VEffectDeclaration of id
  | VEffectPattern of expression

module Interpret (M : MONAD_FAIL) : sig
  val run : expression list -> (value, error_message) M.t
end = struct
  open M

  let find map key =
    match Map.find map key with
    | Some v -> return v
    | None -> fail (String.concat [ "Runtime error: unbound value "; key; "." ])
  ;;

  let find_effect set effect_name =
    match Set.exists set ~f:(Poly.( = ) effect_name) with
    | true -> return true
    | false -> fail (String.concat [ "Runtime error: unbound effect "; effect_name; "." ])
  ;;

  let update_ids environment key value =
    { environment with ids = Map.update environment.ids key ~f:(fun _ -> value) }
  ;;

  let update_effects environment effect_name =
    { environment with effects = Set.add environment.effects effect_name }
  ;;

  let update_effect_handlers environment key value =
    { environment with
      effect_handlers = Map.update environment.effect_handlers key ~f:(fun _ -> value)
    }
  ;;

  let extend_effect_handlers environment_with_effects environment =
    { environment with effect_handlers = environment_with_effects.effect_handlers }
  ;;

  let empty =
    { ids = Map.empty (module String)
    ; effects = Set.empty (module String)
    ; effect_handlers = Map.empty (module String)
    }
  ;;

  let rec eval (expression : expression) (environment : environment) =
    let rec foldr f ini = function
      | [] -> return ini
      | h :: t ->
        let* head = eval h environment in
        let* tail = foldr f ini t in
        return @@ f head tail
    in
    let ( = ) = Poly.( = )
    and ( <> ) x y = not @@ Poly.( = ) x y
    and ( > ) = Poly.( > )
    and ( < ) = Poly.( < )
    and ( >= ) = Poly.( >= )
    and ( <= ) = Poly.( <= ) in
    match expression with
    | ELiteral literal ->
      (match literal with
       | LInt x -> return @@ VInt x
       | LString x -> return @@ VString x
       | LBool x -> return @@ VBool x
       | LChar x -> return @@ VChar x
       | LUnit -> return VUnit)
    | EBinaryOperation (operation, left_operand, right_operand) ->
      let* left_operand = eval left_operand environment in
      let* right_operand = eval right_operand environment in
      let rec vadt_predicate left_operand right_operand =
        match left_operand, right_operand with
        | VADT (x_name, x_data), VADT (y_name, y_data) ->
          if not (x_name = y_name)
          then return @@ false
          else (
            match x_data, y_data with
            | None, None -> return @@ true
            | None, _ | _, None -> return @@ false
            | Some (VInt x), Some (VInt y) -> return @@ (x = y)
            | Some (VString x), Some (VString y) -> return @@ (x = y)
            | Some (VBool x), Some (VBool y) -> return @@ (x = y)
            | Some (VChar x), Some (VChar y) -> return @@ (x = y)
            | Some (VList x), Some (VList y) -> return @@ (x = y)
            | Some (VADT (x_name, x_data)), Some (VADT (y_name, y_data)) ->
              vadt_predicate (VADT (x_name, x_data)) (VADT (y_name, y_data))
            | _ -> fail "Runtime error: mismatching types.")
        | _ -> fail "Runtime error: this predicate should be only used for VADT types."
      in
      (match operation, left_operand, right_operand with
       (* Operations on ADT *)
       | Eq, VADT (x_name, x_data), VADT (y_name, y_data) ->
         vadt_predicate (VADT (x_name, x_data)) (VADT (y_name, y_data))
         >>= fun x -> return @@ VBool x
       | NEq, VADT (x_name, x_data), VADT (y_name, y_data) ->
         vadt_predicate (VADT (x_name, x_data)) (VADT (y_name, y_data))
         >>= fun x -> return @@ VBool (not x)
       | _, VADT (_, _), VADT (_, _) -> fail "Runtime error: unsupported operation."
       (* Arithmetic operations *)
       | Add, VInt x, VInt y -> return @@ VInt (x + y)
       | Sub, VInt x, VInt y -> return @@ VInt (x - y)
       | Mul, VInt x, VInt y -> return @@ VInt (x * y)
       | Div, VInt x, VInt y ->
         if y = 0 then fail "Runtime error: division by zero" else return @@ VInt (x / y)
       | (Add | Sub | Mul | Div), _, _ ->
         fail "Runtime error: operands were expected of type int."
       (* Equality *)
       | Eq, VInt x, VInt y -> return @@ VBool (x = y)
       | Eq, VString x, VString y -> return @@ VBool (x = y)
       | Eq, VBool x, VBool y -> return @@ VBool (x = y)
       | Eq, VChar x, VChar y -> return @@ VBool (x = y)
       | Eq, VList x, VList y -> return @@ VBool (x = y)
       | Eq, _, _ -> fail "Runtime error: unsupported operation"
       (* TODO VADT equality comparison *)
       (* Inequality *)
       | NEq, VInt x, VInt y -> return @@ VBool (x <> y)
       | NEq, VString x, VString y -> return @@ VBool (x <> y)
       | NEq, VBool x, VBool y -> return @@ VBool (x <> y)
       | NEq, VChar x, VChar y -> return @@ VBool (x <> y)
       | NEq, VList x, VList y -> return @@ VBool (x <> y)
       | NEq, _, _ -> fail "Runtime error: unsupported operation"
       (* TODO VADT inequality comparison *)
       (* Greater than ( > ) *)
       | GT, VInt x, VInt y -> return @@ VBool (x > y)
       | GT, VBool x, VBool y -> return @@ VBool (x > y)
       | GT, VString x, VString y -> return @@ VBool (x > y)
       | GT, VChar x, VChar y -> return @@ VBool (x > y)
       | GT, VList x, VList y -> return @@ VBool (x > y)
       | GT, _, _ -> fail "Runtime error: unsupported operation"
       (* Less then ( < ) *)
       | LT, VInt x, VInt y -> return @@ VBool (x < y)
       | LT, VBool x, VBool y -> return @@ VBool (x < y)
       | LT, VString x, VString y -> return @@ VBool (x < y)
       | LT, VChar x, VChar y -> return @@ VBool (x < y)
       | LT, VList x, VList y -> return @@ VBool (x < y)
       | LT, _, _ -> fail "Runtime error: unsupported operation"
       (* Greater than or equal ( >= ) *)
       | GTE, VInt x, VInt y -> return @@ VBool (x >= y)
       | GTE, VBool x, VBool y -> return @@ VBool (x >= y)
       | GTE, VString x, VString y -> return @@ VBool (x >= y)
       | GTE, VChar x, VChar y -> return @@ VBool (x >= y)
       | GTE, VList x, VList y -> return @@ VBool (x >= y)
       | GTE, _, _ -> fail "Runtime error: unsupported operation"
       (* Less then or equal ( <= ) *)
       | LTE, VInt x, VInt y -> return @@ VBool (x <= y)
       | LTE, VBool x, VBool y -> return @@ VBool (x <= y)
       | LTE, VString x, VString y -> return @@ VBool (x <= y)
       | LTE, VChar x, VChar y -> return @@ VBool (x <= y)
       | LTE, VList x, VList y -> return @@ VBool (x <= y)
       | LTE, _, _ -> fail "Runtime error: unsupported operation"
       (* And ( && ) *)
       | AND, VBool x, VBool y -> return @@ VBool (x && y)
       | OR, VBool x, VBool y -> return @@ VBool (x || y)
       | (AND | OR), _, _ -> fail "Runtime error: bool type was expected.")
    | EIdentifier name ->
      if name = "_"
      then fail "Runtime error: used wildcard in right-hand expression."
      else
        let* v = find environment.ids name in
        (match v with
         | VFun (id_list, function_body, environment, Recursive) ->
           return
           @@ VFun (id_list, function_body, update_ids environment name v, Recursive)
         | _ -> return v)
    | EApplication (function_expr, argument_expr) ->
      let* eval_argument = eval argument_expr environment in
      let* eval_function = eval function_expr environment in
      let* id_list, function_body, local_environment, recursive =
        match eval_function with
        | VFun (id_list, function_body, environment, recursive) ->
          return (id_list, function_body, environment, recursive)
        | _ -> fail "Runtime error: not a function, cannot be applied."
      in
      let environment = extend_effect_handlers environment local_environment in
      let* id, id_list =
        match id_list with
        | head :: tail -> return (head, tail)
        | _ -> fail "Runtime error: not a function, cannot be applied."
      in
      let environment =
        if id <> "_" then update_ids environment id eval_argument else environment
      in
      if id_list = []
      then eval function_body environment
      else return @@ VFun (id_list, function_body, environment, recursive)
    | EFun (arguments_list, function_body) ->
      (match arguments_list with
       | [] -> eval function_body environment
       | _ -> return @@ VFun (arguments_list, function_body, environment, NonRecursive))
    | EDeclaration (_, arguments_list, function_body) ->
      (match arguments_list with
       | [] -> eval function_body environment
       | _ -> return @@ VFun (arguments_list, function_body, environment, NonRecursive))
    | ERecursiveDeclaration (_, arguments_list, function_body) ->
      (match arguments_list with
       | [] -> eval function_body environment
       | _ -> return @@ VFun (arguments_list, function_body, environment, Recursive))
    | EIf (condition, true_branch, false_branch) ->
      let* eval_conditional = eval condition environment in
      (match eval_conditional with
       | VBool true -> eval true_branch environment
       | VBool false -> eval false_branch environment
       | _ ->
         fail
           "Runtime error: expression was expected of type bool because it is in the \
            condition of an if-statement.")
    | EUnaryOperation (operator, operand) ->
      let* operand = eval operand environment in
      (match operator, operand with
       | Minus, VInt x -> return @@ VInt (-x)
       | Not, VBool x -> return @@ VBool (not x)
       | _ -> fail "Runtime error: mismatching types.")
    | EList list ->
      (match list with
       | [] -> return @@ VList []
       | _ ->
         let rec eval_list list =
           match hd_exn list, tl_exn list with
           | head, [] ->
             let* head = eval head environment in
             return @@ VList [ head ]
           | head, tail ->
             let* head = eval head environment in
             let* tail = eval_list tail in
             (match tail with
              | VList tail ->
                let next_element = hd_exn tail in
                (match head, next_element with
                 | VInt _, VInt _
                 | VString _, VString _
                 | VBool _, VBool _
                 | VChar _, VChar _
                 | VUnit, VUnit
                 | VList _, VList _
                 | VTuple _, VTuple _
                 | VFun (_, _, _, _), VFun (_, _, _, _)
                 | VADT (_, _), VADT (_, _) -> return @@ VList (head :: tail)
                 | _ -> fail "Runtime error: mismatching types in list.")
              | _ -> fail "Runtime error: could not interpret list.")
         in
         eval_list list)
    | EConstructList (operand, list) ->
      let* operand = eval operand environment in
      let* list = eval list environment in
      (match operand, list with
       | VFun (_, _, _, _), _ ->
         fail "Runtime error: unable to place a function into a list."
       | x, VList list -> return @@ VList (x :: list)
       | _ -> fail "Runtime error: mismatching types.")
    | EDataConstructor (constructor_name, content) ->
      (match content with
       | Some data ->
         let* data = eval data environment in
         return @@ VADT (constructor_name, Some data)
       | None -> return @@ VADT (constructor_name, None))
    | ETuple list ->
      let* list = foldr (fun x xs -> x :: xs) [] list in
      return @@ VTuple list
    | ELetIn (bindings_list, expression) ->
      let rec eval_bindings environment = function
        | h :: t ->
          let* result = eval h environment in
          (match h with
           | EDeclaration (name, _, _) ->
             eval_bindings (update_ids environment name result) t
           | ERecursiveDeclaration (name, _, _) ->
             eval_bindings (update_ids environment name result) t
           | _ -> fail "Runtime error: declaration was expected.")
        | _ -> eval expression environment
      in
      eval_bindings environment bindings_list
    | EMatchWith (matched_expression, case_list) ->
      let* environment =
        List.fold_right
          case_list
          ~f:(fun (case, action) environment ->
            let* environment = environment in
            match case with
            | EEffectPattern effect ->
              (match effect with
               | EEffectNoArg name ->
                 let* _ = find_effect environment.effects name in
                 return @@ update_effect_handlers environment name (EFun ([], action))
               | EEffectArg (name, expression) ->
                 let* _ = find_effect environment.effects name in
                 return
                 @@ update_effect_handlers
                      environment
                      name
                      (EFun
                         ( [ "eFFectValuE" ]
                         , EMatchWith (EIdentifier "eFFectValuE", [ expression, action ])
                         ))
               | _ -> fail "Runtime error: not an effect.")
            | _ -> return environment)
          ~init:(return environment)
      in
      let rec compare_patterns matched_expression case action environment =
        let rec helper environment = function
          | matched_head :: matched_tail, head :: tail ->
            let result, environment, head_success =
              compare_patterns
                matched_head
                head
                (EFun ([ "_" ], ELiteral LUnit))
                environment
            in
            let monadic_execution, new_environment, tail_success =
              helper environment (matched_tail, tail)
            in
            result *> monadic_execution, new_environment, head_success && tail_success
          | [], [] -> eval action environment, environment, true
          | _ -> fail "Runtime error: pattern-matching failed.", environment, false
        in
        match matched_expression, case with
        | VInt value, ELiteral (LInt x) when value = x ->
          eval action environment, environment, true
        | VChar value, ELiteral (LChar x) when value = x ->
          eval action environment, environment, true
        | VBool value, ELiteral (LBool x) when value = x ->
          eval action environment, environment, true
        | VString value, ELiteral (LString x) when value = x ->
          eval action environment, environment, true
        | VUnit, ELiteral LUnit -> eval action environment, environment, true
        | VInt value, EUnaryOperation (Minus, ELiteral (LInt x)) when value = -x ->
          eval action environment, environment, true
        | value, EIdentifier id ->
          let new_environment =
            if id <> "_" then update_ids environment id value else environment
          in
          eval action new_environment, new_environment, true
        | VList matched_list, EList list -> helper environment (matched_list, list)
        | VTuple matched_tuple, ETuple tuple -> helper environment (matched_tuple, tuple)
        | VList matched_list, EConstructList (head, tail) ->
          (match matched_list with
           | matched_head :: matched_tail ->
             let result, environment, head_success =
               compare_patterns
                 matched_head
                 head
                 (EFun ([ "_" ], ELiteral LUnit))
                 environment
             in
             let monadic_execution, new_environment, tail_success =
               compare_patterns (VList matched_tail) tail action environment
             in
             result *> monadic_execution, new_environment, head_success && tail_success
           | [] -> fail "Runtime error: pattern-matching failed.", environment, false)
        | VADT (matched_name, value), EDataConstructor (name, expression) ->
          if matched_name <> name
          then fail "Runtime error: pattern-matching failed.", environment, false
          else (
            match value, expression with
            | Some value, Some expression ->
              compare_patterns value expression action environment
            | _ -> fail "Runtime error: pattern-matching failed.", environment, false)
        | _ -> fail "Runtime error: pattern-matching failed.", environment, false
      in
      let* eval_matched_expression = eval matched_expression environment in
      let rec helper lst =
        match lst with
        | case :: tail ->
          let result, _, success =
            compare_patterns eval_matched_expression (fst case) (snd case) environment
          in
          if success then result else helper tail
        | [] -> fail "Runtime error: pattern-matching is not exhaustive."
      in
      helper
        (List.filter case_list ~f:(function
          | EEffectPattern _, _ -> false
          | _ -> true))
    | EEffectDeclaration (name, _) -> return (VEffectDeclaration name)
    | EEffectNoArg name -> return (VEffectNoArg name)
    | EEffectArg (name, expression) -> return @@ VEffectArg (name, expression)
    | EPerform expression ->
      let* eval_expression = eval expression environment in
      (match eval_expression with
       | VEffectNoArg effect_name ->
         let* handler = find environment.effect_handlers effect_name in
         eval handler environment
       | VEffectArg (effect_name, argument) ->
         let* handler = find environment.effect_handlers effect_name in
         eval (EApplication (handler, argument)) environment
       | _ -> fail "Runtime error: not an effect.")
    | EContinue expression -> eval expression environment
    | EEffectPattern expression ->
      let* eval_expression = eval expression environment in
      return eval_expression
  ;;

  let run (program : expression list) =
    let environment = empty in
    let rec helper environment = function
      | [ h ] -> eval h environment
      | h :: t ->
        let* result = eval h environment in
        (match h with
         | EDeclaration (name, _, _) -> helper (update_ids environment name result) t
         | ERecursiveDeclaration (name, _, _) ->
           helper (update_ids environment name result) t
         | EEffectDeclaration (name, _) -> helper (update_effects environment name) t
         | _ -> fail "Runtime error: declaration was expected.")
      | _ -> return VUnit
    in
    helper environment program
  ;;
end

module InterpretResult = Interpret (struct
  include Base.Result

  let ( let* ) m f = bind m ~f
  let ( *> ) l r = l >>= fun _ -> r
end)

(* let rec factorial n acc = if n <= 1 then acc else factorial (n - 1) (acc * n) *)
(* let main = factorial 5 1 *)
let test_program =
  [ ERecursiveDeclaration
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
          (EApplication (EIdentifier "factorial", ELiteral (LInt 5)), ELiteral (LInt 1))
      )
  ]
;;

let%test _ =
  match InterpretResult.run test_program with
  | Base.Result.Ok (VInt 120) -> true
  | _ -> false
;;

let test_program =
  [ EDeclaration
      ( "f"
      , [ "x"; "y"; "z" ]
      , EBinaryOperation
          (Add, EBinaryOperation (Mul, EIdentifier "x", EIdentifier "y"), EIdentifier "z")
      )
  ; EDeclaration
      ( "main"
      , []
      , EApplication
          ( EApplication
              (EApplication (EIdentifier "f", ELiteral (LInt 5)), ELiteral (LInt 10))
          , ELiteral (LInt 10) ) )
  ]
;;

let%test _ = Poly.( = ) (InterpretResult.run test_program) @@ Result.Ok (VInt 60)

let test_program =
  [ EDeclaration
      ( "f"
      , [ "x" ]
      , EIf
          ( EBinaryOperation (Eq, EIdentifier "x", ELiteral (LBool true))
          , ELiteral (LInt 1)
          , ELiteral (LInt 2) ) )
  ; EDeclaration ("main", [], EApplication (EIdentifier "f", ELiteral (LBool false)))
  ]
;;

let%test _ = Poly.( = ) (InterpretResult.run test_program) @@ Result.Ok (VInt 2)

let test_program =
  [ EDeclaration
      ("f", [ "x"; "y" ], EBinaryOperation (Add, EIdentifier "x", EIdentifier "y"))
  ; EDeclaration
      ( "main"
      , []
      , EApplication
          (EApplication (EIdentifier "f", ELiteral (LInt 5)), ELiteral (LInt 10)) )
  ]
;;

let%test _ = Poly.( = ) (InterpretResult.run test_program) @@ Result.Ok (VInt 15)
let%test _ = Poly.( = ) (InterpretResult.run []) @@ Result.Ok VUnit

let test_program =
  [ EDeclaration
      ("main", [], EList [ ELiteral (LInt 2); ELiteral (LInt 3); ELiteral (LInt (-5)) ])
  ]
;;

let%test _ =
  Poly.( = ) (InterpretResult.run test_program)
  @@ Result.Ok (VList [ VInt 2; VInt 3; VInt (-5) ])
;;

let test_program =
  [ EDeclaration
      ( "main"
      , []
      , EList
          [ EList [ ELiteral (LChar 'c'); ELiteral (LChar 'f') ]
          ; EList [ ELiteral (LChar 'h'); ELiteral (LChar 'g') ]
          ] )
  ]
;;

let%test _ =
  Poly.( = ) (InterpretResult.run test_program)
  @@ Result.Ok (VList [ VList [ VChar 'c'; VChar 'f' ]; VList [ VChar 'h'; VChar 'g' ] ])
;;

let test_program =
  [ EDeclaration
      ( "main"
      , []
      , EConstructList
          (ELiteral (LInt 2), EList [ ELiteral (LInt 2); ELiteral (LInt (-10)) ]) )
  ]
;;

let%test _ =
  Poly.( = ) (InterpretResult.run test_program)
  @@ Result.Ok (VList [ VInt 2; VInt 2; VInt (-10) ])
;;

let test_program =
  [ EDeclaration
      ( "main"
      , []
      , EConstructList
          ( EDataConstructor ("Ok", Some (ELiteral (LBool true)))
          , EList
              [ EDataConstructor ("Ok", Some (ELiteral (LBool false)))
              ; EDataConstructor ("Error", Some (ELiteral (LString "failed")))
              ] ) )
  ]
;;

let%test _ =
  Poly.( = ) (InterpretResult.run test_program)
  @@ Result.Ok
       (VList
          [ VADT ("Ok", Some (VBool true))
          ; VADT ("Ok", Some (VBool false))
          ; VADT ("Error", Some (VString "failed"))
          ])
;;

let test_program =
  [ EDeclaration
      ( "main"
      , []
      , ETuple
          [ ELiteral (LChar 'f')
          ; EBinaryOperation (Add, ELiteral (LInt 2), ELiteral (LInt (-2)))
          ] )
  ]
;;

let%test _ =
  Poly.( = ) (InterpretResult.run test_program)
  @@ Result.Ok (VTuple [ VChar 'f'; VInt 0 ])
;;

let test_program =
  [ EDeclaration
      ( "main"
      , []
      , ELetIn
          ( [ EDeclaration ("x", [], ELiteral (LInt 1)) ]
          , EBinaryOperation (Add, EIdentifier "x", ELiteral (LInt (-2))) ) )
  ]
;;

let%test _ = Poly.( = ) (InterpretResult.run test_program) @@ Result.Ok (VInt (-1))

let test_program =
  [ EDeclaration
      ( "phi"
      , [ "n" ]
      , ELetIn
          ( [ ERecursiveDeclaration
                ( "helper"
                , [ "last1"; "last2"; "n" ]
                , EIf
                    ( EBinaryOperation (GT, EIdentifier "n", ELiteral (LInt 0))
                    , EApplication
                        ( EApplication
                            ( EApplication (EIdentifier "helper", EIdentifier "last2")
                            , EBinaryOperation
                                (Add, EIdentifier "last1", EIdentifier "last2") )
                        , EBinaryOperation (Sub, EIdentifier "n", ELiteral (LInt 1)) )
                    , EIdentifier "last2" ) )
            ]
          , EApplication
              ( EApplication
                  ( EApplication (EIdentifier "helper", ELiteral (LInt 1))
                  , ELiteral (LInt 1) )
              , EBinaryOperation (Sub, EIdentifier "n", ELiteral (LInt 2)) ) ) )
  ; EDeclaration ("main", [], EApplication (EIdentifier "phi", ELiteral (LInt 10)))
  ]
;;

let%test _ = Poly.( = ) (InterpretResult.run test_program) @@ Result.Ok (VInt 55)

let test_program =
  [ EDeclaration
      ( "main"
      , []
      , EMatchWith
          ( ETuple [ ELiteral (LInt 1); ELiteral (LInt 2) ]
          , [ ETuple [ EIdentifier "x"; ELiteral (LInt 2) ], ELiteral (LBool true) ] ) )
  ]
;;

let%test _ = Poly.( = ) (InterpretResult.run test_program) @@ Result.Ok (VBool true)

let test_program =
  [ EDeclaration
      ( "main"
      , []
      , EMatchWith
          ( ETuple [ ELiteral (LInt 1); ELiteral (LInt 2) ]
          , [ ( ETuple [ EIdentifier "x"; EIdentifier "y" ]
              , EList [ EIdentifier "x"; EIdentifier "y" ] )
            ] ) )
  ]
;;

let%test _ =
  Poly.( = ) (InterpretResult.run test_program) @@ Result.Ok (VList [ VInt 1; VInt 2 ])
;;

let test_program =
  [ EDeclaration
      ( "head"
      , []
      , EFun
          ( [ "list" ]
          , EMatchWith
              ( EIdentifier "list"
              , [ ( EConstructList (EIdentifier "h", EIdentifier "_")
                  , EDataConstructor ("Some", Some (EIdentifier "h")) )
                ; EIdentifier "_", EDataConstructor ("None", None)
                ] ) ) )
  ; EDeclaration
      ( "main"
      , []
      , EApplication (EIdentifier "head", EList [ ELiteral (LInt 2); ELiteral (LInt 3) ])
      )
  ]
;;

let%test _ =
  Poly.( = ) (InterpretResult.run test_program)
  @@ Result.Ok (VADT ("Some", Some (VInt 2)))
;;

let rec print result =
  let open Format in
  let rec print_list delimiter = function
    | [ head ] -> print head
    | head :: tail ->
      print head;
      printf "%c " delimiter;
      print_list delimiter tail
    | [] -> ()
  in
  match result with
  | VInt value -> printf "%d" value
  | VChar value -> printf "%C" value
  | VBool value -> printf "%B" value
  | VString value -> printf "%S" value
  | VUnit -> printf "()"
  | VList list ->
    printf "[";
    print_list ';' list;
    printf "]"
  | VTuple tuple ->
    printf "(";
    print_list ',' tuple;
    printf ")"
  | VADT (name, argument) ->
    printf "%s " name;
    (match argument with
     | Some argument -> print argument
     | None -> ())
  | VFun _ -> printf "Not a value."
;;

let test_program =
  [ EEffectDeclaration ("EmptyListEffect", TEffect (TList (TVar 0)))
  ; EDeclaration
      ( "safe_head"
      , []
      , EFun
          ( [ "list" ]
          , EMatchWith
              ( EIdentifier "list"
              , [ EConstructList (EIdentifier "h", EIdentifier "_"), EIdentifier "h"
                ; EIdentifier "_", EPerform (EEffectNoArg "EmptyListEffect")
                ] ) ) )
  ; EDeclaration
      ( "main"
      , []
      , EMatchWith
          ( EApplication
              (EIdentifier "safe_head", EList [ ELiteral (LInt 2); ELiteral (LInt 3) ])
          , [ EEffectPattern (EEffectNoArg "EmptyListEffect"), EList []
            ; EIdentifier "hd", EIdentifier "hd"
            ] ) )
  ]
;;

let%expect_test _ =
  let result = InterpretResult.run test_program in
  match result with
  | Ok result -> print result
  | Error err ->
    Format.printf "%s\n" err;
    [%expect {|
  2
  |}]
;;

let test_program =
  [ EEffectDeclaration ("EmptyListEffect", TEffect (TList (TVar 0)))
  ; EDeclaration
      ( "safe_head"
      , []
      , EFun
          ( [ "list" ]
          , EMatchWith
              ( EIdentifier "list"
              , [ EConstructList (EIdentifier "h", EIdentifier "_"), EIdentifier "h"
                ; EIdentifier "_", EPerform (EEffectNoArg "EmptyListEffect")
                ] ) ) )
  ; EDeclaration
      ( "main"
      , []
      , EMatchWith
          ( EApplication (EIdentifier "safe_head", EList [])
          , [ EEffectPattern (EEffectNoArg "EmptyListEffect"), EList []
            ; EIdentifier "hd", EIdentifier "hd"
            ] ) )
  ]
;;

let%expect_test _ =
  let result = InterpretResult.run test_program in
  match result with
  | Ok result -> print result
  | Error err ->
    Format.printf "%s\n" err;
    [%expect {|
  []
  |}]
;;
