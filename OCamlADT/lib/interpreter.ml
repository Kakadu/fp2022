(** Copyright 2021-2022, Kakadu, EmirVildanov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Evaluation (interpetation module) *)

open Ast
open Printer
open Typing

(* Errors [exceptions] that may occur during interpetation *)
exception UnacceptableMatchExpresstion of expr
exception NotImplementedYet of expr
exception InexhaustiveMatch of expr
exception RuntimeError of string
exception TypeError of expr * Typing.t
exception UnboundVariable of string

(** Helper function for comparing Values *)
let rec vcompare v1 v2 =
  match v1, v2 with
  | VBool b1, VBool b2 -> compare b1 b2
  | VInt n1, VInt n2 -> compare n1 n2
  | VNil, VNil -> 0
  | VCons _, VNil -> 1
  | VNil, VCons _ -> -1
  | VCons (h1, tl1), VCons (h2, tl2) ->
    let c = compare h1 h2 in
    if c = 0 then vcompare tl1 tl2 else c
  | _ ->
    let msg =
      Printf.sprintf
        "The comparison of the values %s and %s resulted in failure."
        (val_to_string v1)
        (val_to_string v2)
    in
    raise (RuntimeError msg)
;;

(** Helper function for combining environments *)
let concat (env1 : environment) (env2 : environment) : environment =
  IdMap.fold (fun x v e -> IdMap.add x v e) env1 env2
;;

(** Helper function evaluation Match epxression.
    
    let match_example x = match x with
   | h::_ -> (match h with
              | h::_ -> print_endline h [HERE new occurences of "h" appears -> that's why we
                                         need to pass envitonment every time]
              | _ -> print_endline "Inner other")
   | _    -> print_endline "Outer other" *)
let rec pattern_match (v : value) (pattern : expr) : bool * environment =
  let emptyMap = IdMap.empty in
  match pattern with
  | Constant const ->
    (match const, v with
     | Bool b1, VBool b2 -> b1 = b2, emptyMap
     | Int n1, VInt n2 -> n1 = n2, emptyMap
     | Nil, VNil -> true, emptyMap
     | Unit, VUnit -> true, emptyMap
     | _ -> false, emptyMap)
  | Var name -> true, IdMap.add name v emptyMap
  | Cons (p1, p2) ->
    (match v with
     | VCons (v1, v2) ->
       (match pattern_match v1 p1, pattern_match v2 p2 with
        | (b1, env1), (b2, env2) ->
          if b1 && b2 then true, concat env1 env2 else false, emptyMap)
     | _ -> false, emptyMap)
  | _ -> raise (UnacceptableMatchExpresstion pattern)
;;

(** Hepler function for updating value in environment *)
let update (x : id) (v : value) (env : environment) : environment =
  IdMap.mapi (fun key value -> if key = x then v else value) env
;;

(* try
    IdMap.find x env := v;
    env
  with
  | Not_found -> env *)

(** Main evaluation function *)
let rec eval (e : expr) (env : environment) : evaluation_result =
  match e with
  | Constant c ->
    (match c with
     | Bool b -> { value = VBool b; env }
     | Int n -> { value = VInt n; env }
     | Ast.Str s -> { value = VString s; env }
     | Nil -> { value = VNil; env }
     | Unit -> { value = VUnit; env })
  | Var name ->
    (try { value = IdMap.find name env; env } with
     | Not_found -> raise (UnboundVariable name))
  | Fun (id, expr) -> { value = VClosure (env, id, expr); env }
  | BinaryOp (_, _, _) -> { value = eval_operator env e; env }
  | UnaryOp (_, _) -> { value = eval_operator env e; env }
  | Cons (expr1, expr2) ->
    { value = VCons ((eval expr1 env).value, (eval expr2 env).value); env }
  | IfThenElse (if_expr, then_expr, else_expr) ->
    (match (eval if_expr env).value with
     | VBool true -> { value = (eval then_expr env).value; env }
     | VBool false -> { value = (eval else_expr env).value; env }
     | _ -> raise (TypeError (if_expr, BaseT Typing.Bool)))
  | Let (flag, id, _, expr) ->
    (match flag with
     | true ->
       (match expr with
        | Fun (fun_id, expr) ->
          { value = VUnit; env = IdMap.add id (VRecClosure (id, env, fun_id, expr)) env }
        | _ -> { value = VUnit; env = IdMap.add id (eval expr env).value env })
     | false -> { value = VUnit; env = IdMap.add id (eval expr env).value env })
  | LetIn (let_expr, in_expr) -> eval in_expr (eval let_expr env).env
  | App (expr1, expr2) ->
    let expr1_eval_value = (eval expr1 env).value in
    (match expr1_eval_value with
     | VClosure (envi, id, expr) ->
       { value = (eval expr (IdMap.add id (eval expr2 env).value envi)).value; env }
     | VRecClosure (name, envi, id, expr) ->
       let expr2_eval_value = (eval expr2 env).value in
       let extended_with_expr2_env = IdMap.add id expr2_eval_value envi in
       let extended_with_func_name_env =
         IdMap.add name expr1_eval_value extended_with_expr2_env
       in
       { value = (eval expr extended_with_func_name_env).value; env }
     | _ -> raise (RuntimeError "Evaluation of function did not give back a VClosure"))
  | Match (expr, patterns) ->
    let expr_value = (eval expr env).value in
    let rec helper = function
      | [] -> raise (InexhaustiveMatch expr)
      | (from_expr, to_expr) :: tl ->
        (match pattern_match expr_value from_expr with
         | corresponds, match_env ->
           let eval_res =
             if corresponds then eval to_expr (concat match_env env) else helper tl
           in
           eval_res)
    in
    helper patterns
  | Tuple _ -> raise (NotImplementedYet e)
  | ADT (name, _) when name = nil_adt_name -> { value = VNil; env }
  | ADT (name, exprs) when name = cons_adt_name ->
    (match exprs with
     | [ x; y ] -> { value = VCons ((eval x env).value, (eval y env).value); env }
     | _ -> raise (RuntimeError "Unacceptable List ADT instance!"))
  | ADT (_, _) -> raise (NotImplementedYet e)
  | Type (_, _) -> raise (NotImplementedYet e)

and eval_operator env = function
  | BinaryOp (op, expr1, expr2) ->
    (match op with
     | Plus -> eval_arithmetic expr1 expr2 env ( + )
     | Minus -> eval_arithmetic expr1 expr2 env ( - )
     | Mult -> eval_arithmetic expr1 expr2 env ( * )
     | Divide -> eval_arithmetic expr1 expr2 env ( / )
     | Eq -> eval_compare expr1 expr2 env ( = ))
  | UnaryOp (_, e) ->
    (match (eval e env).value with
     | VInt n -> VInt (-n)
     | _ -> raise (RuntimeError "Unary operator can only be applied to Int"))
  | _ as expr ->
    let msg =
      Printf.sprintf
        "The expression: %s\n is not an operator, but an operator was expected."
        (expr_to_string expr)
    in
    raise (RuntimeError msg)

and eval_arithmetic first second env op =
  match (eval first env).value, (eval second env).value with
  | VInt n1, VInt n2 -> VInt (op n1 n2)
  | VInt _, _ -> raise (TypeError (second, BaseT Int))
  | _ -> raise (TypeError (first, BaseT Int))

and eval_compare first second env op =
  VBool (op (vcompare (eval first env).value (eval second env).value) 0)
;;

let string_of_ast_eval ast = val_to_string (eval ast IdMap.empty).value

let%expect_test "Single Unit constant" =
  Printf.printf "%s" (string_of_ast_eval (Constant Unit));
  [%expect {| () |}]
;;

let%expect_test "Single Int constant" =
  Printf.printf "%s" (string_of_ast_eval (Constant (Int 69)));
  [%expect {| 69 |}]
;;

let%expect_test "Single Let expr with Int assignment" =
  Printf.printf
    "%s"
    (string_of_ast_eval (LetIn (Let (false, "x", None, Constant (Int 69)), Var "x")));
  [%expect {| 69 |}]
;;

let%expect_test "Match with wrong typing" =
  Printf.printf
    "%s"
    (string_of_ast_eval
       (Match
          ( Constant (Int 1)
          , [ Constant (Int 2), Constant (Int 1); Constant (Int 1), Constant (Str "One") ]
          )));
  [%expect {| One |}]
;;
