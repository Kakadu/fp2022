(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Builtinops
open Environment
open Utils

type var_type =
  | Local
  | Class

let get_var_type (name : string) : var_type =
  if String.starts_with ~prefix:"@" name then Class else Local
;;

let rec eval (st : state) (code : ast) : value * state =
  let prepare_params (raw_params : ast list) (st : state) : value list * state =
    let params, n_st =
      List.fold_left
        (fun (acc, betw_exp_st) el ->
          match eval betw_exp_st el with
          | s_v, s_s -> s_v :: acc, s_s)
        ([], st)
        raw_params
    in
    List.rev params, n_st
  in
  match code with
  | Literal (lit_t, v) -> value_of_literal lit_t v, st
  | Var n -> get_variable st n, st
  | VarAssign (i, v) ->
    let var_value, st = eval st v in
    let new_state =
      match get_var_type i with
      | Local -> set_local_var st i var_value
      | Class -> set_class_var st i var_value
    in
    var_value, new_state
  | Binop (op, l, r) ->
    let op_f = match_binop op in
    let l_v, st = eval st l in
    let r_v, st = eval st r in
    op_f l_v r_v, st
  | Conditional (cond, thenB, elseB) ->
    let cond_v, st = eval st cond in
    eval st (conditional cond_v thenB elseB)
  | Seq lst ->
    List.fold_left (fun (_, betw_exp_st) el -> eval betw_exp_st el) (Nil, st) lst
  | WhileLoop (cond, body) ->
    let rec iteration s =
      let c_v, n_st = eval s cond in
      match c_v with
      | Bool v when v ->
        let _, n_st = eval n_st body in
        iteration n_st
      | Bool v when not v -> n_st
      | _ -> typefail "While loop expected bool as condition"
    in
    Nil, iteration st
  | ArrayDecl lst ->
    let values, new_st =
      List.fold_left
        (fun (acc, betw_exp_st) el ->
          match eval betw_exp_st el with
          | s_v, s_s -> s_v :: acc, s_s)
        ([], st)
        lst
    in
    Array (ref (List.rev values)), new_st
  | Indexing (box, ind) ->
    let b_v, n_st = eval st box in
    let i_v, n_st = eval n_st ind in
    index_get b_v i_v, n_st
  | FuncDeclaration (level, name, params, body) -> (
    let func = Function (name, params, eval_function name params body) in
    match level with
    | TopLevel -> Nil, set_local_var st name func
    | Method -> Nil, set_class_var st name func
  )
  | MethodAccess (obj, meth, params) ->
    let params, n_st = prepare_params params st in
    (match eval n_st obj with
     | obj, n_st -> process_method_access obj meth params n_st)
  | Invocation (box_inv, params) ->
    let left, n_st = eval st box_inv in
    let params, n_st = prepare_params params n_st in
    (match left with
     | Function (_, _, f) -> f n_st params, n_st
     | _ -> typefail "")
  | IndexAssign (box, index, new_value) ->
    let box_v, n_st = eval st box in
    let index_v, n_st = eval n_st index in
    let new_v, n_st = eval n_st new_value in
    index_set box_v index_v new_v, n_st
  | ClassDeclaration (name, members) ->
    let class_state =
      List.fold_left
        (fun cls_st method_code ->
          let dumb_state = add_class_scope st cls_st in
          match method_code with
          | FuncDeclaration (Method, _, _, _) | VarAssign (_, _) ->
            (* Doesnt need to manually add because dumb_state is mutable*)
            let _, _ = eval dumb_state method_code in
            cls_st 
          | _ -> failwith "Wrong field in class declaration")
        (ref empty_class_state)
        members
    in
    let class_state = set_in_class_state !class_state name (Class !class_state) in
    let new_class: value = Class class_state in
    let new_st = set_local_var st name new_class in
    Nil, new_st

and eval_function
  (f_name : string)
  (p_names : string list)
  (body : ast)
  (st: state)
  (p_values : value list)
  : value
  =
  let () =
    if not (List.length p_names = List.length p_values)
    then failwith "Wrong number of arguments."
  in
  let state =
    set_local_var
      st
      f_name
      (Function (f_name, p_names, eval_function f_name p_names body))
  in
  let params = List.combine p_names p_values in
  let step st (n, v) = set_local_var st n v in
  let initiated = List.fold_left step state params in
  fst (eval initiated body)

and process_method_access
  (obj : value)
  (m_name : string)
  (params : value list)
  (st : state)
  : value * state
  =
  let method_not_exist (class_name : string) =
    failwith ("Method" ^ m_name ^ "does not exist for" ^ class_name)
  in
  match obj with
  | Bool b ->
    (match m_name with
     | "class" -> if b then String "TrueClass", st else String "FalseClass", st
     | "inspect" | "to_s" -> String (string_of_bool b), st
     | _ -> method_not_exist (if b then "TrueClass" else "FalseClass"))
  | Integer i ->
    (match m_name with
     | "class" -> String "Integer", st
     | "abs" -> Integer (abs i), st
     | "digits" ->
       ( Array
           (i
           |> string_of_int
           |> String.to_seq
           |> List.of_seq
           |> List.map (String.make 1)
           |> List.map (fun s -> String s)
           |> ref)
       , st )
     | _ -> method_not_exist "Integer")
  | String s ->
    (match m_name with
     | "class" -> String "String", st
     | "length" -> Integer (String.length s), st
     | "starts_with" ->
       ( Bool
           (match params with
            | String pref :: [] -> String.starts_with ~prefix:pref s
            | _ -> failwith "Wrong number of arguments or wrong types")
       , st )
     | "ends_with" ->
       ( Bool
           (match params with
            | String suff :: [] -> String.ends_with ~suffix:suff s
            | _ -> failwith "Wrong number of arguments or wrong types")
       , st )
     | _ -> method_not_exist "String")
  | Array arr ->
    (match m_name with
     | "class" -> String "Array", st
     | "to_s" ->
       String ("[" ^ String.concat ", " (List.map string_of_value !arr) ^ "]"), st
     | "length" | "size" -> Integer (List.length !arr), st
     | _ -> method_not_exist "Array")
  | Function (name, param_list, _) ->
    (match m_name with
     | "to_s" ->
       ( String
           (String.concat
              ""
              [ "<Function: "; name; "("; String.concat ", " param_list; ")"; ">" ])
       , st )
     | _ -> method_not_exist "Function")
  | Nil ->
    (match m_name with
     | "class" -> String "NilClass", st
     | _ -> method_not_exist "NilClass")
  | Class init_state -> (
    let initialize_class: unit -> value = fun () ->
    let class_state = ref init_state in
      match get_from_class_state init_state "initialize" with
      (* f will mutate class state *)
      | Function( _, _, f) -> let _ = f (add_class_scope st class_state) params in ClassInstance class_state
      | _ -> failwith "initialize must be a function"
    in 
    try initialize_class (), st with
      Failure (_) -> ClassInstance (ref init_state), st
  )
  | ClassInstance cls_state -> (
    let enriched_state = add_class_scope st cls_state in
    match get_from_class_state !cls_state m_name with
    (** f will mutate added shared state*)
    | Function (_, _, f) -> f enriched_state params, st
    | _ -> method_not_exist "ClassInstance"
  )
;;

let run (code : ast) = fst (eval Stdlib.initial_state code)
let run_expr s = s |> Parser.parse |> run |> Utils.string_of_value
let test_eval prog exp = String.equal (run_expr prog) exp

let%test "integer" = test_eval "123" "123"
let%test "plus" = test_eval "1 + 1" "2"
let%test "minus" = test_eval "1 - 1" "0"
let%test "multiply" = test_eval "4 * 4" "16"
let%test "division" = test_eval "42 / 6" "7"
let%test "binop ws around" = test_eval " 1 + 1 " "2"
let%test "binop new lines around" = test_eval "\n1+1\n" "2"
let%test "multiple binops" = test_eval "1 + 2 * 3" "7"
let%test "binops with brackets" = test_eval "(1 + 4) * (2 + 3) / 5" "5"
let%test "bool and" = test_eval "true && false" "false"
let%test "bool or" = test_eval "true || false" "true"
let%test "int comp eq" = test_eval "6 == 7" "false"
let%test "int comp neq" = test_eval "6 != 7" "true"
let%test "simple string" = test_eval "\"hello\"" "hello"
let%test "repeated string" = test_eval "\"hello\"*3" "hellohellohello"
let%test "string comparison" = test_eval "\"hello\" == \"hello\"" "true"
let%test "simple conditional" = test_eval "if true then 10 else 7 end" "10"
let%test "conditional with binop" = test_eval "if 10 <= 7 then 1 else 2 end" "2"
let%test "expr in condition" = test_eval "if 10 + 3 == 13 then 10 else 7 end" "10"
let%test "no else branch in conditional" = test_eval "if false then 10 end" "nil"
let%test "multiple expr sep by newline" = test_eval "1 + 1\n2 + 2\n3 + 3" "6"
let%test "multiple expr sep by semicolumn" = test_eval "1 + 1;2 + 2;3 + 3" "6"
let%test "multiple expr with random ws" = test_eval "1 + 1; 2 + 2\n 5 + 5" "10"

let%test "conditional with multuple expressions" =
  test_eval "if true then 1 + 2; 2 + 3; 5 + 5 end" "10"
;;

let%test "sum of conditionals" = test_eval "if true then 10 end + if true then 5 end" "15"
let%test "condition with gr and nl" = test_eval "if 3 > 2 then 6 end" "6"
let%test "variable assign" = test_eval "u = 2 + 2" "4"
let%test "variable assign itself" = test_eval "x = 10; x = x + 1; x" "11"
let%test "variable assign and call" = test_eval "x = 10; 2 + 2; x" "10"
let%test "multiple variables and call" = test_eval "x = 10; y = 7; x + y" "17"
let%test "bool variables" = test_eval "x = true; y = false; x && y" "false"

let%test "string variables" =
  test_eval "x = \"hello \"; y = \"world\"; x + y" "hello world"
;;

let%test "variable from condition" =
  test_eval "x = false; y = if x then 13 else 10 end; y" "10"
;;

let%test "while loop" = test_eval "while false do 10 end" "nil"

let%test "while loop with variables" =
  test_eval "x = 0; while x < 10 do \n x = x + 1 \n end; x" "10"
;;

let%test "empty array" = test_eval "[]" "[]"
let%test "int array declaration" = test_eval "[1, 2, 3]" "[1, 2, 3]"
let%test "bool array declaration" = test_eval "[false, true]" "[false, true]"
let%test "mixed array declaration" = test_eval "[1 + 1, false || true]" "[2, true]"
let%test "array sum" = test_eval "[1, 2] + [3, 4]" "[1, 2, 3, 4]"
let%test "array times int" = test_eval "[1, 2] * 3" "[1, 2, 1, 2, 1, 2]"

let%test "array equality" =
  test_eval "[1, true, \"hello\"] == [1, true, \"hello\"]" "true"
;;

let%test "variable assign to array" = test_eval "x = [1, 2]; x" "[1, 2]"

let%test "using variables inside array" =
  test_eval "x = 10; y = [1, 2, x]; y" "[1, 2, 10]"
;;

let%test "indexing array" = test_eval "[1, 2, 3, 4][1]" "2"
let%test "indexing variable" = test_eval "x = [1, 3, 4]; x[1]" "3"
let%test "indexing string" = test_eval "\"Hello\"[2]" "l"
let%test "indexing expression" = test_eval "([1, 2] + [3, 4])[2]" "3"
let%test "index set" = test_eval "x = [1, 2, 3]; x[2] = 0; x" "[1, 2, 0]"

let%test "index set between variables" =
  test_eval "x = [1, 2, 3]; y = x; y[0] = 4; x" "[4, 2, 3]"
;;

let%test "index set in 2d array" =
  test_eval "x = [[1], 2, 3]; (x[0])[0] = 4; x" "[[4], 2, 3]"
;;

let%test "one arg function" = test_eval "def f(x)\nx+1\nend; f(10)" "11"
let%test "multiple args function" = test_eval "def f(x, y)\nx - y\nend; f(10, 3)" "7"

let%test "factorial" =
  test_eval
    "def f(i)\n x=1 \n while i > 0 \n x = x * i; i = i - 1 \n end \n x \n end; f(5)"
    "120"
;;

let%test "int class field" = test_eval "123.class ()" "Integer"
let%test "int expr abs field" = test_eval "(2 - 5).abs ()" "3"
let%test "" = test_eval "true.to_s ()" "true"
let%test "method acess" = test_eval "\"Hello world\".starts_with (\"Hello\")" "true"
let%test "method access in expression" = test_eval "(1 - 3).abs () + 4" "6"
let%test "method access from variable" = test_eval "x = 10; x.class ()" "Integer"
let%test "method access from array" = test_eval "([1, 2, 3, 4]).length ()" "4"
let%test "class variables in same scope" = test_eval "@x = 10; @x" "10"

let%test "class variables in func scope" =
  test_eval "@x = 10; def f\n @x \n end; f ()" "10"
;;
