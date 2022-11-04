open Ast
open Base
open List

module type MONAD_FAIL = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

type error_message = string
type number_of_arguments = int

type environment = (id, value, Base.String.comparator_witness) Base.Map.t

and value =
  | VInt of int
  | VString of string
  | VBool of bool
  | VChar of char
  | VUnit
  | VList of value list
  | VTuple of value list
  | VFun of
      id list
      * expression
      * environment
      * number_of_arguments (* expression contains body of the function *)
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
    and ( <= ) = Poly.( >= ) in
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
      let rec value_is_equal (VADT (x_name, x_data)) (VADT (y_name, y_data)) =
        x_name = y_name
        && (for_all ~f:(function
              | VInt x, VInt y -> x = y
              | VString x, VString y -> x = y
              | VBool x, VBool y -> x = y
              | VChar x, VChar y -> x = y
              | VList x, VList y -> x = y
              | VADT (x_name, x_data), VADT (y_name, y_data) ->
                value_is_equal (VADT (x_name, x_data)) (VADT (y_name, y_data))
              | _, _ -> false)
           @@ zip_exn x_data y_data)
      in
      let value_check_types (VADT (_, x_data)) (VADT (_, y_data)) =
        for_all ~f:(function
          | VInt _, VInt _ -> true
          | VString _, VString _ -> true
          | VBool _, VBool _ -> true
          | VChar _, VChar _ -> true
          | VList _, VList _ -> true
          | VADT (_, _), VADT (_, _) -> true
          | _, _ -> false)
        @@ zip_exn x_data y_data
      in
      (match operation, left_operand, right_operand with
      (* Operations on ADT *)
      | Eq, VADT (x_name, x_data), VADT (y_name, y_data) ->
        if not @@ value_check_types (VADT (x_name, x_data)) (VADT (y_name, y_data))
        then fail "Runtime error: mismatching types."
        else
          return
          @@ VBool
               (x_name = y_name
               && value_is_equal (VADT (x_name, x_data)) (VADT (y_name, y_data)))
      | NEq, VADT (x_name, x_data), VADT (y_name, y_data) ->
        if not @@ value_check_types (VADT (x_name, x_data)) (VADT (y_name, y_data))
        then fail "Runtime error: mismatching types."
        else
          return
          @@ VBool
               (x_name <> y_name
               && (not @@ value_is_equal (VADT (x_name, x_data)) (VADT (y_name, y_data)))
               )
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
      | Some v -> return v
      | None -> fail "Runtime error: unbound value.")
    | EApplication (function_expr, argument_expr) ->
      let* eval_function = eval function_expr environment in
      let* eval_argument = eval argument_expr environment in
      let* id_list, function_body, environment, number_of_args =
        match eval_function with
        | VFun (id_list, function_body, environment, number_of_args) ->
          return (id_list, function_body, environment, number_of_args)
        | _ -> fail "Runtime error: not a function, cannot be applied."
      in
      let* id, id_list =
        match hd id_list, tl id_list with
        | Some v1, Some v2 -> return (v1, v2)
        | _ -> fail "Runtime error: not a function, cannot be applied."
      in
      let environment = Map.update environment id ~f:(fun _ -> eval_argument)
      and number_of_args = number_of_args - 1 in
      if number_of_args == 0
      then eval function_body environment
      else return @@ VFun (id_list, function_body, environment, number_of_args)
    | EFun (arguments_list, function_body) ->
      return
      @@ VFun (arguments_list, function_body, environment, List.length arguments_list)
    | EDeclaration (_, arguments_list, function_body) ->
      return
      @@ VFun (arguments_list, function_body, environment, List.length arguments_list)
    | ERecursiveDeclaration (name, arguments_list, function_body) ->
      let rec fix f = f (fix f) in
      let result =
        fix
        @@ fun self ->
        VFun
          ( arguments_list
          , function_body
          , Map.update environment name ~f:(fun _ -> self)
          , List.length arguments_list )
      in
      return result
    | EIf (condition, true_branch, false_branch) ->
      let* eval_conditional = eval condition environment in
      (match eval_conditional with
      | VBool true ->
        let* eval_true_branch = eval true_branch environment in
        return eval_true_branch
      | VBool false ->
        let* eval_false_branch = eval false_branch environment in
        return eval_false_branch
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