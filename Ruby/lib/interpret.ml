(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Utils

type var_type =
  | Local
  | Class

let get_var_type (name : string) : var_type =
  if String.starts_with ~prefix:"@" name then Class else Local
;;

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module type MONADERROR = sig
  include MONAD

  val error : string -> 'a t
end

module Result = struct
  type 'a t = ('a, string) Result.t

  let ( >>= ) = Result.bind
  let return = Result.ok
  let error = Result.error
end

module Eval (M : MONADERROR) = struct
  open M
  open Ast

  let empty_class_state : class_state = Base.Map.empty (module Base.String)

  let set_in_class_state (st : class_state) (name : string) (new_v : value) : class_state =
    Base.Map.set st ~key:name ~data:new_v
  ;;

  let empty_state : state =
    { local_vars = Base.Map.empty (module Base.String)
    ; class_scopes = [ ref (Base.Map.empty (module Base.String)) ]
    }
  ;;

  let from_global (st : state) : state =
    { local_vars = empty_state.local_vars; class_scopes = st.class_scopes }
  ;;

  let set_local_var (st : state) (name : string) (new_v : value) : state t =
    return { local_vars = Base.Map.set st.local_vars ~key:name ~data:new_v
    ; class_scopes = st.class_scopes
    }
  ;;

  let add_class_scope (st : state) (init_state : class_state ref) : state =
    { local_vars = st.local_vars; class_scopes = [ init_state ] @ st.class_scopes }
  ;;

  let get_class_var (st : state) (name : string) : value t =
    let rec get_from_map_stack = function
      | [] -> error "Variable does not exist"
      | m :: tail ->
        (match Base.Map.find !m name with
         | Some v -> return v
         | None -> get_from_map_stack tail)
    in
    get_from_map_stack st.class_scopes
  ;;

  let get_variable (st : state) (name : string) : value t =
    match Base.Map.find st.local_vars name with
    | Some v -> return v
    | None -> get_class_var st name
  ;;

  let get_from_class_state (cls_state : class_state) (name : string) : value t =
    get_variable (add_class_scope empty_state (ref cls_state)) name
  ;;

  let set_class_var (st : state) (name : string) (new_v : value) =
    match Base.List.hd st.class_scopes with
    | Some cur_class ->
      cur_class := Base.Map.set !cur_class ~key:name ~data:new_v;
      return { local_vars = st.local_vars; class_scopes = st.class_scopes }
    | None -> error "Class scopes are empty"
  ;;

  let binop_typefail (op : string) (l : value) (r : value) =
    error
      (String.concat
         ""
         [ "No candidates for "
         ; op
         ; " with arguments: "
         ; string_of_value l
         ; " and "
         ; string_of_value r
         ])
  ;;

  let plus x y =
    match x, y with
    | Integer x, Integer y -> return (Integer (x + y))
    | String x, String y -> return (String (String.cat x y))
    | Array x, Array y -> return (Array (ref (!x @ !y)))
    | _ -> binop_typefail "+" x y
  ;;

  let minus x y =
    match x, y with
    | Integer x, Integer y -> return (Integer (x - y))
    | _ -> binop_typefail "-" x y
  ;;

  let multiply x y =
    match x, y with
    | Integer x, Integer y -> return (Integer (x * y))
    | String x, Integer y ->
      return (String (List.init y (fun _ -> x) |> String.concat ""))
    | Array x, Integer y ->
      return (Array (ref (List.init y (fun _ -> !x) |> List.concat)))
    | _ -> binop_typefail "*" x y
  ;;

  let divide x y =
    match x, y with
    | Integer x, Integer y -> return (Integer (x / y))
    | _ -> binop_typefail "/" x y
  ;;

  let raw_eq x y =
    match x, y with
    | Integer x, Integer y -> return (x = y)
    | Bool x, Bool y -> return (x = y)
    | String x, String y -> return (String.equal x y)
    | Array x, Array y -> return (x = y)
    | Nil, Nil -> return true
    | _ -> binop_typefail "=" x y
  ;;

  let eq x y = raw_eq x y >>= fun v -> return (Bool v)
  let neq x y = raw_eq x y >>= fun v -> return (Bool (not v))

  let and_op x y =
    match x, y with
    | Bool x, Bool y -> return (Bool (x && y))
    | _ -> binop_typefail "&&" x y
  ;;

  let or_op x y =
    match x, y with
    | Bool x, Bool y -> return (Bool (x || y))
    | _ -> binop_typefail "||" x y
  ;;

  let raw_gr x y =
    match x, y with
    | Integer x, Integer y -> return (x > y)
    | String x, String y -> return (String.compare x y > 0)
    | _ -> binop_typefail ">" x y
  ;;

  let gr x y = raw_gr x y >>= fun v -> return (Bool v)

  let gr_eq x y =
    raw_gr x y >>= fun v1 -> raw_eq x y >>= fun v2 -> return (Bool (v1 || v2))
  ;;

  let ls_eq x y = raw_gr x y >>= fun v -> return (Bool (not v))

  let ls x y =
    raw_gr x y >>= fun v1 -> raw_eq x y >>= fun v2 -> return (Bool (not (v1 || v2)))
  ;;

  let match_binop = function
    | "+" -> return plus
    | "-" -> return minus
    | "*" -> return multiply
    | "/" -> return divide
    | "==" -> return eq
    | "!=" -> return neq
    | "&&" -> return and_op
    | "||" -> return or_op
    | ">" -> return gr
    | ">=" -> return gr_eq
    | "<=" -> return ls_eq
    | "<" -> return ls
    | op -> return (fun _ _ -> error ("Unknown binop " ^ op))
  ;;

  let conditional (c : value) (t : ast) (e : ast) =
    match c with
    | Bool c -> if c then return t else return e
    | _ -> error "Conditional expects bool as condition"
  ;;

  let index_get (v : value) (ind : value) =
    match v, ind with
    | Array v, Integer i -> return (List.nth !v i)
    | String v, Integer i -> return (Ast.String (String.get v i |> String.make 1))
    | _ -> binop_typefail "index" v ind
  ;;

  let replace l pos a = List.mapi (fun i x -> if i = pos then a else x) l

  let index_set (v : value) (ind : value) (new_v : value) : value t =
    match v, ind, new_v with
    | Array v, Integer i, x ->
      v := replace !v i x;
      return (Array v)
    | _ -> error "Wrong arguments in index set"
  ;;

  let rec eval (st : state) (code : ast) : (value * state) t =
    let eval_multiple (codes : ast list) (st : state) : (value list * state) t =
      let eval_step (mon : (value list * state) t) (code : ast) =
        mon
        >>= fun (acc, st) ->
        eval st code >>= fun (new_v, new_st) -> return (new_v :: acc, new_st)
      in
      List.fold_left eval_step (return ([], st)) codes
    in
    match code with
    | Literal (lit_t, v) -> return (value_of_literal lit_t v, st)
    | Var n -> get_variable st n >>= fun v -> return (v, st)
    | VarAssign (i, v) ->
      eval st v
      >>= fun (var_value, st) ->
      let new_state =
        match get_var_type i with
        | Local -> set_local_var st i var_value
        | Class -> set_class_var st i var_value
      in
      new_state >>= fun new_state ->
      return (var_value, new_state)
    | Binop (op, l, r) ->
      match_binop op
      >>= fun op_f ->
      eval st l
      >>= fun (l_v, st) ->
      eval st r >>= fun (r_v, st) -> op_f l_v r_v >>= fun op_res -> return (op_res, st)
    | Conditional (cond, thenB, elseB) ->
      eval st cond
      >>= fun (cond_v, st) ->
      conditional cond_v thenB elseB >>= fun branch -> eval st branch
    | Seq lst ->
      (match lst with
       | [] -> return (Nil, st)
       | lst ->
         eval_multiple lst st
         >>= fun (v_lst, new_st) -> return (v_lst |> List.hd, new_st))
    | WhileLoop (cond, body) ->
      let rec iteration s =
        eval s cond
        >>= fun (c_v, n_st) ->
        match c_v with
        | Bool v when v -> eval n_st body >>= fun (_, n_st) -> iteration n_st
        | Bool v when not v -> return n_st
        | _ -> error "While loop expected bool as condition"
      in
      iteration st >>= fun new_st -> return (Nil, new_st)
    | ArrayDecl lst ->
      eval_multiple lst st
      >>= fun (arr_v, new_st) -> return (Array (ref (List.rev arr_v)), new_st)
    | Indexing (box, ind) ->
      eval st box
      >>= fun (b_v, n_st) ->
      eval n_st ind >>= fun (i_v, n_st) -> index_get b_v i_v >>= fun v -> return (v, n_st)
    | FuncDeclaration (level, name, params, body) ->
      let func = Function (name, params, body) in
      (match level with
       | TopLevel -> set_local_var st name func >>= fun n_st -> return (Nil, n_st)
       | Method -> set_class_var st name func >>= fun n_st -> return (Nil, n_st))
    | MethodAccess (obj, meth, params) ->
      eval_multiple params st
      >>= fun (params, n_st) ->
      eval n_st obj
      >>= fun (obj, n_st) ->
      process_method_access obj meth params n_st >>= fun v -> return (v, n_st)
    | Invocation (box_inv, params) ->
      eval st box_inv
      >>= fun (left, n_st) ->
      eval_multiple params n_st
      >>= fun (params, n_st) ->
      (match left with
       | Function (name, param_names, body) ->
         eval_function name param_names body (from_global n_st) params
         >>= fun v -> return (v, n_st)
       | _ -> error "Only function can be invoked")
    | IndexAssign (box, index, new_value) ->
      eval st box
      >>= fun (box_v, n_st) ->
      eval n_st index
      >>= fun (index_v, n_st) ->
      eval n_st new_value
      >>= fun (new_v, n_st) -> index_set box_v index_v new_v >>= fun v -> return (v, n_st)
    | ClassDeclaration (name, members) ->
      let is_initialize = function
        | FuncDeclaration (Method, "initialize", _, _) -> true
        | _ -> false
      in
      let members =
        match List.find_opt is_initialize members with
        | Some _ -> members
        | None -> FuncDeclaration (Method, "initialize", [], Seq []) :: members
      in
      let class_state = ref empty_class_state in
      (* dumb state will mutate class state through ref *)
      let dumb_state = add_class_scope (from_global st) class_state in
      eval_multiple members dumb_state
      >>= fun (_, _) ->
      let new_class : value = Class !class_state in
       set_local_var st name new_class >>= fun new_st -> return (Nil, new_st)

  and eval_function
    (f_name : string)
    (p_names : string list)
    (body : ast)
    (st : state)
    (p_values : value list)
    : value t
    =
    if not (List.length p_names = List.length p_values)
    then error "Wrong number of arguments."
    else (
      let state = set_local_var st f_name (Function (f_name, p_names, body)) in
      let params = List.combine p_names (List.rev p_values) in
      let step st (n, v) = st>>= fun st -> set_local_var st n v in
      let initiated = List.fold_left step state params in
      initiated >>= fun initiated -> eval initiated body >>= fun (v, _) -> return v)

  and process_method_access
    (obj : value)
    (m_name : string)
    (params : value list)
    (st : state)
    : value t
    =
    let method_not_exist (class_name : string) =
      error ("Method " ^ m_name ^ " does not exist for " ^ class_name)
    in
    let st = from_global st in
    match obj with
    | Bool b ->
      (match m_name with
       | "class" ->
         if b then return (String "TrueClass") else return (String "FalseClass")
       | "inspect" | "to_s" -> return (String (string_of_bool b))
       | _ -> method_not_exist (if b then "TrueClass" else "FalseClass"))
    | Integer i ->
      (match m_name with
       | "class" -> return (String "Integer")
       | "abs" -> return (Integer (abs i))
       | "digits" ->
         return
           (Array
              (i
              |> string_of_int
              |> String.to_seq
              |> List.of_seq
              |> List.map (String.make 1)
              |> List.map (fun s -> String s)
              |> ref))
       | _ -> method_not_exist "Integer")
    | String s ->
      (match m_name with
       | "class" -> return (String "String")
       | "length" -> return (Integer (String.length s))
       | "starts_with" ->
        (match params with
        | String pref :: [] -> return (Bool(String.starts_with ~prefix:pref s))
        | _ -> error "Wrong number of arguments or wrong types")
       | "ends_with" ->
              (match params with
               | String suff :: [] -> return (Bool(String.ends_with ~suffix:suff s))
               | _ -> error "Wrong number of arguments or wrong types")
       | _ -> method_not_exist "String")
    | Array arr ->
      (match m_name with
       | "class" -> return (String "Array")
       | "to_s" ->
         return (String ("[" ^ String.concat ", " (List.map string_of_value !arr) ^ "]"))
       | "length" | "size" -> return (Integer (List.length !arr))
       | _ -> method_not_exist "Array")
    | Function (name, param_list, _) ->
      (match m_name with
       | "to_s" ->
         return
           (String
              (String.concat
                 ""
                 [ "<Function: "; name; "("; String.concat ", " param_list; ")"; ">" ]))
       | _ -> method_not_exist "Function")
    | Nil ->
      (match m_name with
       | "class" -> return (String "NilClass")
       | _ -> method_not_exist "NilClass")
    | Class init_state ->
      get_from_class_state init_state "initialize"
      >>= fun init_func ->
      (match init_func with
       | Function (name, param_names, body) ->
         let class_state = ref init_state in
         let st = add_class_scope st class_state in
         eval_function name param_names body st params
         >>= fun _ -> return (ClassInstance class_state)
       | _ -> error "initialize must be a function")
    | ClassInstance cls_state -> (
      let enriched_scope = add_class_scope st cls_state in
      get_from_class_state !cls_state m_name >>= fun v -> 
        match v with 
        | Function (name, param_names, body) -> eval_function name param_names body enriched_scope params
        | _ -> method_not_exist "ClassInstance"
    )
  ;;
end

let eval_code (code: ast): string =
  let module E = Eval (Result) in
  match E.eval E.empty_state code with
  | Ok v -> string_of_value (fst v)
  | Error s -> "Error: " ^ s
  
let run_expr s = s |> Parser.parse |> eval_code
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

let%test "stateless class" =
  test_eval "class Hello \n def hello \n 10 \n end \n end \n h = Hello.new() \n h.hello()" "10"

let%test "statefull class" = 
  test_eval "class Hello \n @x = 10 \n def get_x \n @x \n end \n end \n h = Hello.new() \n h.get_x()" "10"
