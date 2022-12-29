(** Copyright 2021-2022, Danila Pechenev & Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open List

module type MONAD_FAIL = sig
  include Base.Monad.S2

  val run : ('a, 'e) t -> ok:('a -> ('b, 'e) t) -> err:('e -> ('b, 'e) t) -> ('b, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  val ( *> ) : ('a, 'e) t -> ('a, 'e) t -> ('a, 'e) t
end

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
  | VInt of int (** 5 *)
  | VString of string (** "apple" *)
  | VBool of bool (** true *)
  | VChar of char (** 'a' *)
  | VUnit (** () *)
  | VList of value list (** [1; 2; 3] *)
  | VTuple of value list (** ("abc", 123, false) *)
  | VFun of id list * expression * environment * rec_flag (** fun x -> x * x *)
  | VADT of data_constructor_name * value option (** Some 100 *)
  | VEffectNoArg of id (** EmptyListEffect *)
  | VEffectArg of id * expression (** E 0 *)
  | VEffectDeclaration of id (** effect SmallDiscount : int -> int effect *)
  | VEffectPattern of expression (** | effect EmptyListEffect -> ... *)
  | VEffectHandler of id * value

type error =
  | UnboundValue of string (** Unbound value *)
  | UnboundEffect of string (** Inbound effect *)
  | Unreachable
      (** Unreachable code. If this error is thrown then something went seriously wrong *)
  | UnsupportedOperation (** Used unsupported operation *)
  | Division_by_zero (** n / 0*)
  | NotAFunction (** Unreachable when type inference is used *)
  | TypeMismatch (** Unreachable when type inference is used *)
  | MisusedWildcard (** Wildcard is in the right-hand expression *)
  | NotAnEffect (** Perform was applied not to an effect *)
  | PatternMatchingFailed (** The case is not matched *)
  | NonExhaustivePatternMatching (** Pattern-matching is not exhaustive *)
  | ContinuationFailure of value

module Environment (M : MONAD_FAIL) = struct
  open M

  let find map key =
    match Base.Map.find map key with
    | Some value -> return value
    | None -> fail (UnboundValue key)
  ;;

  let find_effect set effect_name =
    match Base.Set.exists set ~f:(Base.Poly.( = ) effect_name) with
    | true -> return true
    | false -> fail (UnboundEffect effect_name)
  ;;

  let update_ids environment key value =
    { environment with ids = Base.Map.update environment.ids key ~f:(fun _ -> value) }
  ;;

  let update_effects environment effect_name =
    { environment with effects = Base.Set.add environment.effects effect_name }
  ;;

  let update_effect_handlers environment key value =
    { environment with
      effect_handlers =
        Base.Map.update environment.effect_handlers key ~f:(fun _ -> value)
    }
  ;;

  let extend_effect_handlers environment_with_effects environment =
    { environment with effect_handlers = environment_with_effects.effect_handlers }
  ;;

  let empty =
    { ids = Base.Map.empty (module Base.String)
    ; effects = Base.Set.empty (module Base.String)
    ; effect_handlers = Base.Map.empty (module Base.String)
    }
  ;;
end

module Interpret (M : MONAD_FAIL) : sig
  val run : expression list -> (value, error) M.t
end = struct
  open M
  open Environment (M)

  let rec eval (expression : expression) (environment : environment) =
    let rec foldr f ini = function
      | [] -> return ini
      | head :: tail ->
        let* head = eval head environment in
        let* tail = foldr f ini tail in
        return @@ f head tail
    in
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
            | Some (VADT (_, _)), Some (VADT (_, _)) ->
              vadt_predicate left_operand right_operand
            | _ -> fail TypeMismatch)
        | _ -> fail Unreachable
      in
      (match operation, left_operand, right_operand with
       (* Operations on ADT *)
       | Eq, VADT (_, _), VADT (_, _) ->
         vadt_predicate left_operand right_operand
         >>= fun result -> return @@ VBool result
       | NEq, VADT (x_name, x_data), VADT (y_name, y_data) ->
         vadt_predicate (VADT (x_name, x_data)) (VADT (y_name, y_data))
         >>= fun x -> return @@ VBool (not x)
       | _, VADT (_, _), VADT (_, _) -> fail UnsupportedOperation
       (* Arithmetic operations *)
       | Add, VInt x, VInt y -> return @@ VInt (x + y)
       | Sub, VInt x, VInt y -> return @@ VInt (x - y)
       | Mul, VInt x, VInt y -> return @@ VInt (x * y)
       | Div, VInt x, VInt y ->
         if y = 0 then fail Division_by_zero else return @@ VInt (x / y)
       | (Add | Sub | Mul | Div), _, _ -> fail TypeMismatch
       (* And ( && ) *)
       | AND, VBool x, VBool y -> return @@ VBool (x && y)
       | OR, VBool x, VBool y -> return @@ VBool (x || y)
       | (AND | OR), _, _ -> fail TypeMismatch
       (* Equality *)
       | op, arg1, arg2 ->
         let comparison_operation : 'a. 'a -> 'a -> bool =
           match op with
           | Eq -> Base.Poly.( = )
           | NEq -> Base.Poly.( <> )
           | GT -> Base.Poly.( > )
           | LT -> Base.Poly.( < )
           | GTE -> Base.Poly.( >= )
           | _ -> Base.Poly.( <= )
         in
         (match arg1, arg2 with
          | VInt x, VInt y -> return @@ VBool (comparison_operation x y)
          | VString x, VString y -> return @@ VBool (comparison_operation x y)
          | VBool x, VBool y -> return @@ VBool (comparison_operation x y)
          | VChar x, VChar y -> return @@ VBool (comparison_operation x y)
          | VList x, VList y -> return @@ VBool (comparison_operation x y)
          | _, _ -> fail UnsupportedOperation))
    | EIdentifier name ->
      if name = "_"
      then fail MisusedWildcard
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
        | _ -> fail NotAFunction
      in
      let environment = extend_effect_handlers environment local_environment in
      let* id, id_list =
        match id_list with
        | head :: tail -> return (head, tail)
        | _ -> fail NotAFunction
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
       | _ -> fail TypeMismatch)
    | EUnaryOperation (operator, operand) ->
      let* operand = eval operand environment in
      (match operator, operand with
       | Minus, VInt x -> return @@ VInt (-x)
       | Not, VBool x -> return @@ VBool (not x)
       | _ -> fail TypeMismatch)
    | EList list ->
      (match list with
       | [] -> return @@ VList []
       | _ ->
         let rec eval_list list =
           match Base.List.hd_exn list, Base.List.tl_exn list with
           | head, [] ->
             let* head = eval head environment in
             return @@ VList [ head ]
           | head, tail ->
             let* head = eval head environment in
             let* tail = eval_list tail in
             (match tail with
              | VList tail -> return @@ VList (head :: tail)
              | _ -> fail Unreachable)
         in
         eval_list list)
    | EConstructList (operand, list) ->
      let* operand = eval operand environment in
      let* list = eval list environment in
      (match operand, list with
       | x, VList list -> return @@ VList (x :: list)
       | _ -> fail TypeMismatch)
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
           | _ -> fail Unreachable)
        | _ -> eval expression environment
      in
      eval_bindings environment bindings_list
    | EMatchWith (matched_expression, case_list) ->
      let* environment =
        Base.List.fold_right
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
                         ( [ "type" ]
                         , EMatchWith (EIdentifier "type", [ expression, action ]) ))
               | _ -> fail NotAnEffect)
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
          | _ -> fail PatternMatchingFailed, environment, false
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
           | [] -> fail PatternMatchingFailed, environment, false)
        | VADT (matched_name, value), EDataConstructor (name, expression) ->
          if matched_name <> name
          then fail PatternMatchingFailed, environment, false
          else (
            match value, expression with
            | Some value, Some expression ->
              compare_patterns value expression action environment
            | _ -> fail PatternMatchingFailed, environment, false)
        | VEffectHandler (name, _), EEffectPattern pattern ->
          (match pattern with
           | (EEffectArg (id, _) | EEffectNoArg id) when id = name ->
             eval action environment, environment, true
           | _ -> fail PatternMatchingFailed, environment, false)
        | _ -> fail PatternMatchingFailed, environment, false
      in
      let* eval_matched_expression = eval matched_expression environment in
      let rec helper = function
        | case :: tail ->
          let result, _, success =
            compare_patterns eval_matched_expression (fst case) (snd case) environment
          in
          if success then result else helper tail
        | [] -> fail NonExhaustivePatternMatching
      in
      helper case_list
    | EEffectDeclaration (name, _) -> return (VEffectDeclaration name)
    | EEffectNoArg name -> return (VEffectNoArg name)
    | EEffectArg (name, expression) -> return @@ VEffectArg (name, expression)
    | EPerform expression ->
      let* eval_expression = eval expression environment in
      (match eval_expression with
       | VEffectNoArg effect_name ->
         let* handler = find environment.effect_handlers effect_name in
         run
           (eval handler environment)
           ~ok:(fun value -> return (VEffectHandler (effect_name, value)))
           ~err:
             (function
              | ContinuationFailure value -> return value
              | error -> fail error)
       | VEffectArg (effect_name, argument) ->
         let* handler = find environment.effect_handlers effect_name in
         run
           (eval (EApplication (handler, argument)) environment)
           ~ok:(fun value -> return (VEffectHandler (effect_name, value)))
           ~err:
             (function
              | ContinuationFailure value -> return value
              | error -> fail error)
       | _ -> fail NotAnEffect)
    | EContinue expression ->
      run
        (eval expression environment)
        ~ok:(fun value -> fail (ContinuationFailure value))
        ~err:fail
    | EEffectPattern expression ->
      let* eval_expression = eval expression environment in
      return eval_expression
  ;;

  let run (program : expression list) =
    let environment = empty in
    let rec helper environment = function
      | [ head ] -> eval head environment
      | head :: tail ->
        let* result = eval head environment in
        (match head with
         | EDeclaration (name, _, _) -> helper (update_ids environment name result) tail
         | ERecursiveDeclaration (name, _, _) ->
           helper (update_ids environment name result) tail
         | EEffectDeclaration (name, _) -> helper (update_effects environment name) tail
         | _ -> fail Unreachable)
      | _ -> return VUnit
    in
    helper environment program
  ;;
end

module InterpretResult = Interpret (struct
  include Base.Result

  let run x ~ok ~err =
    match x with
    | Ok v -> ok v
    | Error e -> err e
  ;;

  let ( let* ) monad f = bind monad ~f
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

let%test _ = InterpretResult.run test_program = Result.Ok (VInt 60)

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

let%test _ = InterpretResult.run test_program = Result.Ok (VInt 2)

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

let%test _ = InterpretResult.run test_program = Result.Ok (VInt 15)
let%test _ = InterpretResult.run [] = Result.Ok VUnit

let test_program =
  [ EDeclaration
      ("main", [], EList [ ELiteral (LInt 2); ELiteral (LInt 3); ELiteral (LInt (-5)) ])
  ]
;;

let%test _ =
  InterpretResult.run test_program = Result.Ok (VList [ VInt 2; VInt 3; VInt (-5) ])
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
  InterpretResult.run test_program
  = Result.Ok (VList [ VList [ VChar 'c'; VChar 'f' ]; VList [ VChar 'h'; VChar 'g' ] ])
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
  InterpretResult.run test_program = Result.Ok (VList [ VInt 2; VInt 2; VInt (-10) ])
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
  InterpretResult.run test_program
  = Result.Ok
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

let%test _ = InterpretResult.run test_program = Result.Ok (VTuple [ VChar 'f'; VInt 0 ])

let test_program =
  [ EDeclaration
      ( "main"
      , []
      , ELetIn
          ( [ EDeclaration ("x", [], ELiteral (LInt 1)) ]
          , EBinaryOperation (Add, EIdentifier "x", ELiteral (LInt (-2))) ) )
  ]
;;

let%test _ = InterpretResult.run test_program = Result.Ok (VInt (-1))

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

let%test _ = InterpretResult.run test_program = Result.Ok (VInt 55)

let test_program =
  [ EDeclaration
      ( "main"
      , []
      , EMatchWith
          ( ETuple [ ELiteral (LInt 1); ELiteral (LInt 2) ]
          , [ ETuple [ EIdentifier "x"; ELiteral (LInt 2) ], ELiteral (LBool true) ] ) )
  ]
;;

let%test _ = InterpretResult.run test_program = Result.Ok (VBool true)

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

let%test _ = InterpretResult.run test_program = Result.Ok (VList [ VInt 1; VInt 2 ])

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
  Base.Poly.( = ) (InterpretResult.run test_program)
  @@ Result.Ok (VADT ("Some", Some (VInt 2)))
;;
