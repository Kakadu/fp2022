(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Utils

type var_type =
  | Local
  | Class

let get_var_type name = if String.starts_with ~prefix:"@" name then Class else Local

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

  let ( let* ) = ( >>= )
  let empty_class_state : class_state = Base.Map.empty (module Base.String)

  let set_in_class_state st name new_v : class_state =
    Base.Map.set st ~key:name ~data:new_v
  ;;

  let empty_state : state =
    { local_vars = Base.Map.empty (module Base.String)
    ; class_scopes = [ Base.Map.empty (module Base.String) ]
    }
  ;;

  let clear_local st = { empty_state with class_scopes = st.class_scopes }
  let pop_class_scope st = List.hd st.class_scopes

  let set_local_var st name new_v =
    return { st with local_vars = Base.Map.set st.local_vars ~key:name ~data:new_v }
  ;;

  let add_class_scope st init_state =
    { st with class_scopes = init_state :: st.class_scopes }
  ;;

  let get_class_var st name =
    let rec get_from_map_stack = function
      | [] -> error (String.concat " " [ "Variable"; name; "does not exist" ])
      | m :: tail ->
        (match Base.Map.find m name with
         | Some v -> return v
         | None -> get_from_map_stack tail)
    in
    get_from_map_stack st.class_scopes
  ;;

  let get_variable st name =
    match Base.Map.find st.local_vars name with
    | Some v -> return v
    | None -> get_class_var st name
  ;;

  let get_from_class_state cls_state name =
    get_variable (add_class_scope empty_state cls_state) name
  ;;

  let set_class_var st name new_v =
    match st.class_scopes with
    | cur_class :: tail ->
      return
        { st with class_scopes = Base.Map.set cur_class ~key:name ~data:new_v :: tail }
    | [] -> error "Class scopes are empty"
  ;;

  let binop_typefail op l r =
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
    | Array x, Array y -> return (Array (x @ y))
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
    | Array x, Integer y -> return (Array (List.init y (fun _ -> x) |> List.concat))
    | _ -> binop_typefail "*" x y
  ;;

  let divide x y =
    match x, y with
    | Integer x, Integer y -> return (Integer (x / y))
    | _ -> binop_typefail "/" x y
  ;;

  let r_mod x y =
    match x, y with
    | Integer x, Integer y -> return (Integer (x mod y))
    | _ -> binop_typefail "%" x y
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

  let eq x y =
    let* v = raw_eq x y in
    return (Bool v)
  ;;

  let neq x y =
    let* v = raw_eq x y in
    return (Bool (not v))
  ;;

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

  let gr x y =
    let* v = raw_gr x y in
    return (Bool v)
  ;;

  let gr_eq x y =
    let* v1 = raw_gr x y in
    let* v2 = raw_eq x y in
    return (Bool (v1 || v2))
  ;;

  let ls_eq x y =
    let* v = raw_gr x y in
    return (Bool (not v))
  ;;

  let ls x y =
    let* v1 = raw_gr x y in
    let* v2 = raw_eq x y in
    return (Bool (not (v1 || v2)))
  ;;

  let match_binop = function
    | "+" -> return plus
    | "-" -> return minus
    | "*" -> return multiply
    | "/" -> return divide
    | "%" -> return r_mod
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

  let conditional c t e =
    match c with
    | Bool c -> if c then return t else return e
    | _ -> error "Conditional expects bool as condition"
  ;;

  let index_get v ind =
    match v, ind with
    | Array v, Integer i -> return (List.nth v i)
    | String v, Integer i -> return (Ast.String (String.get v i |> String.make 1))
    | _ -> binop_typefail "index" v ind
  ;;

  let replace l pos a = List.mapi (fun i x -> if i = pos then a else x) l

  let rec eval st code =
    let eval_multiple codes st =
      let eval_step mon code =
        let* acc, st = mon in
        let* new_v, new_st = eval st code in
        return (new_v :: acc, new_st)
      in
      let* vl, st = List.fold_left eval_step (return ([], st)) codes in
      return (List.rev vl, st)
    in
    let assign_var st i var_value =
      let new_state =
        match get_var_type i with
        | Local -> set_local_var st i var_value
        | Class -> set_class_var st i var_value
      in
      let* new_st = new_state in
      return (var_value, new_st)
    in
    match code with
    | Literal (lit_t, v) -> return (value_of_literal lit_t v, st)
    | Var n ->
      let* v = get_variable st n in
      return (v, st)
    | VarAssign (i, v) ->
      let* var_value, st = eval st v in
      assign_var st i var_value
    | Binop (op, l, r) ->
      let* op_f = match_binop op in
      let* l_v, st = eval st l in
      let* r_v, st = eval st r in
      let* op_res = op_f l_v r_v in
      return (op_res, st)
    | Conditional (cond, thenB, elseB) ->
      let* cond_v, st = eval st cond in
      let* branch = conditional cond_v thenB elseB in
      eval st branch
    | Seq lst ->
      (match lst with
       | [] -> return (Nil, st)
       | lst ->
         let* v_lst, new_st = eval_multiple lst st in
         let seq_v = v_lst |> List.rev |> List.hd in
         return (seq_v, new_st))
    | WhileLoop (cond, body) ->
      let rec iteration s =
        let* c_v, n_st = eval s cond in
        match c_v with
        | Bool v when v ->
          let* _, n_st = eval n_st body in
          iteration n_st
        | Bool v when not v -> return n_st
        | _ -> error "While loop expected bool as condition"
      in
      let* new_st = iteration st in
      return (Nil, new_st)
    | ArrayDecl lst ->
      let* arr_v, new_st = eval_multiple lst st in
      return (Array arr_v, new_st)
    | Indexing (box, ind) ->
      let* b_v, n_st = eval st box in
      let* i_v, n_st = eval n_st ind in
      let* v = index_get b_v i_v in
      return (v, n_st)
    | FuncDeclaration (level, name, params, body) ->
      (match level with
       | Method ->
         let* n_st = set_class_var st name (Function (name, params, body)) in
         return (Nil, n_st)
         (* Lambda will inherit current local state*)
       | Lambda -> return (Lambda (st, params, body), st))
    | MethodAccess (obj, meth, params) ->
      let* params, n_st = eval_multiple params st in
      let* obj_v, n_st = eval n_st obj in
      let* v, new_class_state = process_method_access obj_v meth params n_st in
      (match obj, obj_v with
       | Var varname, ClassInstance _ ->
         let* _, n_st = assign_var n_st varname (ClassInstance new_class_state) in
         return (v, n_st)
       | _ -> return (v, st))
    | Invocation (box_inv, params) ->
      let* left, n_st = eval st box_inv in
      let* param_v, n_st = eval_multiple params n_st in
      (match left with
       | Function (name, param_names, body) ->
         (* Discard function state entirely *)
         let* v, _ = eval_function name param_names body (clear_local n_st) param_v in
         return (v, n_st)
       | Lambda (closure, param_names, body) ->
         (* Discard lambda state entirely *)
         let* v, _ = eval_function "Lambda" param_names body closure param_v in
         return (v, n_st)
       | _ -> error "Only functions and lambda can be invoked")
    | ClassDeclaration (name, members) ->
      let dumb_state = add_class_scope (clear_local st) empty_class_state in
      let* _, new_st = eval_multiple members dumb_state in
      let new_class = Class (pop_class_scope new_st) in
      set_local_var st name new_class >>= fun new_st -> return (Nil, new_st)

  and eval_function f_name p_names body st p_values =
    if not (List.length p_names = List.length p_values)
    then
      error
        (String.concat
           ""
           [ "Wrong number of arguments in function: "
           ; f_name
           ; "("
           ; String.concat ", " p_names
           ; ")"
           ])
    else (
      let state = set_local_var st f_name (Function (f_name, p_names, body)) in
      let params = List.combine p_names p_values in
      let step st (n, v) =
        let* st = st in
        set_local_var st n v
      in
      let initiated = List.fold_left step state params in
      let* initiated = initiated in
      let* v, st = eval initiated body in
      return (v, st))

  and process_method_access obj m_name params st =
    let method_not_exist class_name =
      error (String.concat "" [ "Method "; m_name; " does not exist for "; class_name ])
    in
    (* Methods always work with empty local state *)
    let st = clear_local st in
    (* Return value with the same class state*)
    let return_sst v = return (v, pop_class_scope st) in
    match obj with
    | Bool b ->
      (match m_name with
       | "class" ->
         if b then return_sst (String "TrueClass") else return_sst (String "FalseClass")
       | "inspect" | "to_s" -> return_sst (String (string_of_bool b))
       | _ -> method_not_exist (if b then "TrueClass" else "FalseClass"))
    | Integer i ->
      (match m_name with
       | "class" -> return_sst (String "Integer")
       | "abs" -> return_sst (Integer (abs i))
       | "digits" ->
         return_sst
           (Array
              (i
              |> string_of_int
              |> Base.String.to_list
              |> List.map (fun s -> String (String.make 1 s))))
       | _ -> method_not_exist "Integer")
    | String s ->
      (match m_name with
       | "class" -> return_sst (String "String")
       | "length" -> return_sst (Integer (String.length s))
       | "starts_with" ->
         (match params with
          (* Accounting for yield sugar*)
          | [ String pref; _ ] -> return_sst (Bool (String.starts_with ~prefix:pref s))
          | _ -> error "Wrong number of arguments or wrong types")
       | "ends_with" ->
         (match params with
          (* Accounting for yield sugar*)
          | [ String suff; _ ] -> return_sst (Bool (String.ends_with ~suffix:suff s))
          | _ -> error "Wrong number of arguments or wrong types")
       | _ -> method_not_exist "String")
    | Array arr ->
      (match m_name with
       | "class" -> return_sst (String "Array")
       | "to_s" ->
         return_sst
           (String (String.concat ", " ([ "[" ] @ List.map string_of_value arr @ [ "]" ])))
       | "length" | "size" -> return_sst (Integer (List.length arr))
       | _ -> method_not_exist "Array")
    | Function (name, param_list, _) ->
      (match m_name with
       | "to_s" ->
         return_sst
           (String
              (String.concat
                 ""
                 [ "<Function: "; name; "("; String.concat ", " param_list; ")"; ">" ]))
       | _ -> method_not_exist "Function")
    | Nil ->
      (match m_name with
       | "class" -> return_sst (String "NilClass")
       | _ -> method_not_exist "NilClass")
    | Class init_state ->
      (match m_name with
       | "new" ->
         get_from_class_state init_state "initialize"
         >>= (function
         | Function (name, param_names, body) ->
           (* Adding class scope *)
           let st = add_class_scope st init_state in
           (* Popping class scope*)
           (* Changes from class variables will only go to new_class_state*)
           let* _, new_st = eval_function name param_names body st params in
           return_sst (ClassInstance (pop_class_scope new_st))
         | _ -> error "initialize must be a function")
       | _ -> method_not_exist "Class")
    | ClassInstance cls_state ->
      (* Adding class scope*)
      let enriched_scope = add_class_scope st cls_state in
      get_from_class_state cls_state m_name
      >>= (function
      | Function (name, param_names, body) ->
        (* Popping class scope *)
        (* Changes from class variables will only go to new_class_state*)
        eval_function name param_names body enriched_scope params
        >>= fun (v, new_st) -> return (v, pop_class_scope new_st)
      | _ -> method_not_exist "ClassInstance")
    | Lambda (_, _, _) -> method_not_exist "Lambda"
  ;;
end

let eval_code code =
  let module E = Eval (Result) in
  match E.eval E.empty_state code with
  | Ok (v, _) -> Ok (string_of_value v)
  | Error s -> Error ("Error: " ^ s)
;;

let run_expr s =
  match
    let open Result in
    s |> Parser.parse >>= eval_code
  with
  | Ok s | Error s -> s
;;

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
  test_eval "@x = 10 \n def f \n   @x \n end \n f ()" "10"
;;

let%test "stateless class" =
  test_eval
    "class Hello \n def hello \n 10 \n end \n end \n h = Hello.new() \n h.hello()"
    "10"
;;

let%test "statefull class" =
  test_eval
    "class Hello \n\
    \   @x = 10 \n\
    \   def get_x \n\
    \     @x \n\
    \   end \n\
    \ end \n\
    \ h = Hello.new() \n\
    \ h.get_x()"
    "10"
;;
