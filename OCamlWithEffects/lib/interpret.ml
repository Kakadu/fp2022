open Ast
open Base
open List

module type MONAD_FAIL = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

type error_message = string
type recursive = bool

type environment = (id, value, Base.String.comparator_witness) Base.Map.t

and value =
  | VInt of int
  | VString of string
  | VBool of bool
  | VChar of char
  | VUnit
  | VList of value list
  | VTuple of value list
  | VFun of id list * expression * environment * recursive
  | VADT of data_constructor_name * value list

module Interpret (M : MONAD_FAIL) : sig
  val run : expression list -> (value, error_message) M.t
end = struct
  open M

  let rec eval expression environment =
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
      let rec vadt_predicate (VADT (x_name, x_data)) (VADT (y_name, y_data)) =
        if not (x_name = y_name)
        then return @@ false
        else (
          let helper x_data y_data =
            match hd x_data, hd y_data with
            | None, None -> return @@ true
            | None, _ | _, None -> return @@ false
            | Some (VInt x), Some (VInt y) -> return @@ (x = y)
            | Some (VString x), Some (VString y) -> return @@ (x = y)
            | Some (VBool x), Some (VBool y) -> return @@ (x = y)
            | Some (VChar x), Some (VChar y) -> return @@ (x = y)
            | Some (VList x), Some (VList y) -> return @@ (x = y)
            | Some (VADT (x_name, x_data)), Some (VADT (y_name, y_data)) ->
              vadt_predicate (VADT (x_name, x_data)) (VADT (y_name, y_data))
            | _, _ -> fail "Runtime error: mismatching types."
          in
          let rec go x_data y_data =
            helper x_data y_data
            >>= fun prev ->
            match tl x_data, tl y_data with
            | None, None -> return true
            | None, _ | _, None -> return false
            | Some x_tail, Some y_tail ->
              let* tail = go x_tail y_tail in
              return (prev && tail)
          in
          go x_data y_data)
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
      (match Map.find environment name with
      | Some v ->
        (match v with
        | VFun (id_list, function_body, environment, true) ->
          return
          @@ VFun
               (id_list, function_body, Map.update environment name ~f:(fun _ -> v), true)
        | _ -> return v)
      | None -> fail "Runtime error: unbound value.")
    | EApplication (function_expr, argument_expr) ->
      let* eval_argument = eval argument_expr environment in
      let* eval_function = eval function_expr environment in
      let* id_list, function_body, environment, recursive =
        match eval_function with
        | VFun (id_list, function_body, environment, recursive) ->
          return (id_list, function_body, environment, recursive)
        | _ -> fail "Runtime error: not a function, cannot be applied."
      in
      let* id, id_list =
        match hd id_list, tl id_list with
        | Some v1, Some v2 -> return (v1, v2)
        | _ -> fail "Runtime error: not a function, cannot be applied."
      in
      let environment = Map.update environment id ~f:(fun _ -> eval_argument) in
      if id_list = []
      then eval function_body environment
      else return @@ VFun (id_list, function_body, environment, recursive)
    | EFun (arguments_list, function_body) ->
      (match arguments_list with
      | [] -> eval function_body environment
      | _ -> return @@ VFun (arguments_list, function_body, environment, false))
    | EDeclaration (_, arguments_list, function_body) ->
      (match arguments_list with
      | [] -> eval function_body environment
      | _ -> return @@ VFun (arguments_list, function_body, environment, false))
    | ERecursiveDeclaration (_, arguments_list, function_body) ->
      (match arguments_list with
      | [] -> eval function_body environment
      | _ -> return @@ VFun (arguments_list, function_body, environment, true))
    | EIf (condition, true_branch, false_branch) ->
      let* eval_conditional = eval condition environment in
      (match eval_conditional with
      | VBool true -> eval true_branch environment
      | VBool false -> eval false_branch environment
      | _ ->
        fail
          "Runtime error: expression was expected of type bool because it is in the \
           condition of an if-statement.")
    | _ -> fail ""
  ;;

  let run (program : expression list) =
    let environment = Map.empty (module Base.String) in
    let rec helper environment = function
      | [ h ] -> eval h environment
      | h :: t ->
        let* result = eval h environment in
        (match h with
        | EDeclaration (name, _, _) ->
          helper (Map.update environment name ~f:(fun _ -> result)) t
        | ERecursiveDeclaration (name, _, _) ->
          helper (Map.update environment name ~f:(fun _ -> result)) t
        | _ -> fail "Runtime error: declaration was expected.")
      | _ -> return VUnit
    in
    helper environment program
  ;;
end

module InterpretResult = Interpret (struct
  include Base.Result

  let ( let* ) m f = bind m ~f
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
  [ EDeclaration ("n", [], ELiteral (LInt 5))
  ; EDeclaration ("main", [], EBinaryOperation (LTE, EIdentifier "n", ELiteral (LInt 1)))
  ]
;;

let%test _ = Poly.( = ) (InterpretResult.run test_program) @@ Result.Ok (VBool false)

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
