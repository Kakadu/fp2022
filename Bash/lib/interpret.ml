(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Utils

module Result : MONAD_FAIL with type 'a t = ('a, string) result = struct
  type 'a t = ('a, string) result

  let return = Result.ok
  let fail = Result.error
  let ( >>= ) = Result.bind
  let ( >>| ) f g = f >>= fun x -> return (g x)

  let ( <|> ) f g =
    match f with
    | Ok _ -> f
    | Error _ -> g ()
  ;;
end

module Interpret (M : MONAD_FAIL) = struct
  open M

  (* simple variables are also indexed arrays *)
  type variable =
    | IndexedArray of string IntMap.t
    | AssocArray of string StrMap.t
  [@@deriving variants, show { with_path = false }]

  (* it is used when executing functions and launching third-party programs *)
  type local_environment = { vars : variable StrMap.t }
  [@@deriving show { with_path = false }]

  type global_environment =
    { vars : variable StrMap.t
    ; functions : group StrMap.t
    ; retcode : int
    }
  [@@deriving show { with_path = false }]

  let default_local_env = { vars = StrMap.empty }
  let default_global_env = { vars = StrMap.empty; functions = StrMap.empty; retcode = 1 }

  type session_env =
    { local : local_environment
    ; global : global_environment
    }
  [@@deriving show { with_path = false }]

  let default_session_env = { local = default_local_env; global = default_global_env }
  let reset_local_env { global } = { local = default_local_env; global }
  let set_local_env env local = { env with local }
  let set_retcode env i = { env with global = { env.global with retcode = i } }

  let get_var_by_name { local; global } name =
    StrMap.find_opt name local.vars
    |> function
    | None -> StrMap.find_opt name global.vars
    | v -> v
  ;;

  let set_var ~is_global { local; global } name v =
    if is_global
    then { local; global = { global with vars = StrMap.add name v global.vars } }
    else { local = { vars = StrMap.add name v local.vars }; global }
  ;;

  let get_fn { global = { functions } } name = StrMap.find_opt name functions

  let set_fn { local; global } name v =
    { local; global = { global with functions = StrMap.add name v global.functions } }
  ;;

  let val_or_empty ~after = function
    | Some v -> after v
    | None -> ""
  ;;

  type var_value =
    { as_num : int
    ; as_str : string
    }

  let get_var_value env { name; subscript } =
    let as_str =
      let after = Fun.id in
      val_or_empty
        ~after:
          (function
           | IndexedArray v ->
             val_or_empty ~after (IntMap.find_opt (int_of_string subscript) v)
           | AssocArray v -> val_or_empty ~after (StrMap.find_opt subscript v))
        (get_var_by_name env name)
    in
    let as_num =
      match int_of_string_opt as_str with
      | None -> 0
      | Some v -> v
    in
    { as_num; as_str }
  ;;

  let eval_param_expansion env = function
    | PositionalParam i ->
      (get_var_value env { name = string_of_int i; subscript = "0" }).as_str
    | VarNameExpansion name ->
      val_or_empty
        ~after:
          (function
           | IndexedArray v -> val_or_empty ~after:Fun.id (IntMap.find_opt 0 v)
           | AssocArray _ -> "")
        (get_var_by_name env name)
    | VarExpansion var -> (get_var_value env var).as_str
    | Length _ -> failwith "Not imlpemeted yet"
    | Substring _ -> failwith "Not imlpemeted yet"
    | SubstrRemoval _ -> failwith "Not imlpemeted yet"
    | Substitute _ -> failwith "Not imlpemeted yet"
  ;;

  (* helper functions to evaluate list's elements *)

  let eval_elems env el_eval list =
    List.fold_left
      (fun acc el ->
        acc >>= fun (env, strs) -> el_eval env el >>| fun (env, str) -> env, str :: strs)
      (return (env, []))
      list
    >>| fun (env, acc) -> env, List.rev acc
  ;;

  let eval_key_value_elems env el_eval list =
    List.fold_left
      (fun acc { key; value } ->
        acc
        >>= fun (env, strs) ->
        el_eval env value >>| fun (env, str) -> env, (key, str) :: strs)
      (return (env, []))
      list
    >>= fun (env, acc) -> return (env, List.rev acc)
  ;;

  let eval_concat env el_eval list =
    eval_elems env el_eval list >>| fun (env, strs) -> env, String.concat "" strs
  ;;

  let rec eval_arithm_expansion env arithm =
    let rec inner env =
      let eval_both lhs rhs =
        inner env lhs
        >>= fun (env, lhs_val) ->
        inner env rhs >>| fun (env, rhs_val) -> env, lhs_val, rhs_val
      in
      let eval_op lhs rhs op =
        eval_both lhs rhs >>| fun (env, lhs_val, rhs_val) -> env, op lhs_val rhs_val
      in
      let eval_slash lhs rhs =
        eval_both lhs rhs
        >>= fun (env, lhs_val, rhs_val) ->
        if rhs_val = 0 then fail "division by 0" else return (env, lhs_val / rhs_val)
      in
      let eval_logical lhs rhs cmp =
        eval_both lhs rhs
        >>| fun (env, lhs_val, rhs_val) -> env, if cmp lhs_val rhs_val then 1 else 0
      in
      let eval_assign env var expr =
        inner env expr
        >>| fun (env, value) ->
        eval_atom_var_assign ~is_global:true (env, var, string_of_int value), value
      in
      function
      | Plus (lhs, rhs) -> eval_op lhs rhs ( + )
      | Minus (lhs, rhs) -> eval_op lhs rhs ( - )
      | Slash (lhs, rhs) -> eval_slash lhs rhs
      | Asterisk (lhs, rhs) -> eval_op lhs rhs ( * )
      | Greater (lhs, rhs) -> eval_logical lhs rhs ( > )
      | GreaterOrEqual (lhs, rhs) -> eval_logical lhs rhs ( >= )
      | Less (lhs, rhs) -> eval_logical lhs rhs ( < )
      | LessOrEqual (lhs, rhs) -> eval_logical lhs rhs ( <= )
      | Equal (lhs, rhs) -> eval_logical lhs rhs ( = )
      | NotEqual (lhs, rhs) -> eval_logical lhs rhs ( != )
      | Variable var -> return (env, (get_var_value env var).as_num)
      | Number num -> return (env, num)
      | Assignment (var, expr) -> eval_assign env var expr
    in
    inner env arithm >>| fun (env, int_res) -> env, int_res

  and eval_atom_string env = function
    | Text str -> return (env, str)
    | ParamExpansion param_exp -> return (env, eval_param_expansion env param_exp)
    | CommandSubstitution _pipe -> failwith "Not implemented yet"
    | ArithmExpansion arithm ->
      eval_arithm_expansion env arithm >>| fun (env, value) -> env, string_of_int value

  and eval_simple_string env = function
    | SingleQuotedString str -> return (env, str)
    | AtomString list | DoubleQuotedString list -> eval_concat env eval_atom_string list

  and eval_single_arg env list = eval_concat env eval_simple_string list

  and eval_arg env = function
    | SingleArg single_arg ->
      eval_single_arg env single_arg >>| fun (env, str) -> env, str :: []
    | MultipleArgs _ -> failwith "Not implemented with"

  and eval_atom_var_assign ~is_global (env, { name; subscript }, str) =
    (match get_var_by_name env name with
     | Some (IndexedArray arr) ->
       IndexedArray (IntMap.add (int_of_string subscript) str arr)
     | Some (AssocArray arr) -> AssocArray (StrMap.add subscript str arr)
     | None -> IndexedArray (IntMap.singleton (int_of_string subscript) str))
    |> set_var ~is_global env name

  and set_env_var env = function
    | AtomVariable { key; value } ->
      eval_single_arg env value
      >>| fun (env, str) -> eval_atom_var_assign ~is_global:true (env, key, str)
    | IndexedArray { key = name; value = list } ->
      eval_elems env eval_arg list
      >>| fun (env, list) ->
      set_var
        ~is_global:true
        env
        name
        (IndexedArray (IntMap.from_list (List.concat list)))
    | AssocArray { key = name; value = list } ->
      eval_key_value_elems env eval_single_arg list
      >>| fun (env, list) ->
      set_var ~is_global:true env name (AssocArray (StrMap.from_list list))

  and eval_cmd env args =
    List.fold_left
      (fun acc arg ->
        acc >>= fun (env, strs) -> eval_arg env arg >>| fun (env, s) -> env, s :: strs)
      (return (env, []))
      args
    >>= fun (env, acc) -> return (env, List.concat (List.rev acc))

  and eval_atom_operand env { invert = _; env_vars; cmd } =
    let rec set_env_vars ~env = function
      | hd :: tl -> set_env_var env hd >>= fun env -> set_env_vars ~env tl
      | [] -> return env
    in
    let get_args session_env tl =
      tl
      |> List.mapi (fun i value ->
           { name = string_of_int (i + 1); subscript = "0" }, value)
      |> List.fold_left
           (fun session_env (var, value) ->
             eval_atom_var_assign ~is_global:false (session_env, var, value))
           session_env
    in
    let eval_function session_env body =
      eval_any_cmds session_env body >>| reset_local_env
    in
    eval_cmd env cmd
    >>= fun (env, args) ->
    match args with
    | hd :: tl ->
      (match get_fn env hd with
       | None -> failwith "Only functions supported"
       | Some body ->
         set_env_vars ~env env_vars
         >>= fun session_env ->
         let { local } = session_env in
         return (reset_local_env session_env)
         >>= fun session_env ->
         eval_function (get_args session_env tl) body
         >>| fun env -> set_local_env env local)
    | [] -> set_env_vars ~env env_vars >>| fun env -> set_retcode env 0

  and eval_operands env =
    let rec eval_operand env =
      let or_and lhs rhs cmp =
        eval_operand env lhs
        >>= fun env -> if cmp env.global.retcode then eval_operand env rhs else return env
      in
      function
      | AtomOperand op -> eval_atom_operand env op
      | OrOperand (lhs, rhs) -> or_and lhs rhs (( = ) 0)
      | AndOperand (lhs, rhs) -> or_and lhs rhs (( != ) 0)
    in
    function
    | hd :: tl -> eval_operand env hd >>= fun env -> eval_operands env tl
    | [] -> return env

  and eval_arithm_compound env arithm =
    eval_arithm_expansion env arithm
    >>| fun (env, value) -> (if value != 0 then 0 else 1) |> set_retcode env

  and eval_until_loop env { condition; cons_cmds } =
    let rec body env retcode =
      eval_any_cmds env condition
      >>= fun env ->
      if env.global.retcode != 0
      then eval_any_cmds env cons_cmds >>= fun env -> body env env.global.retcode
      else return (set_retcode env retcode)
    in
    body env 0

  and eval_while_loop env { condition; cons_cmds } =
    let rec body env retcode =
      eval_any_cmds env condition
      >>= fun env ->
      if env.global.retcode = 0
      then eval_any_cmds env cons_cmds >>= fun env -> body env env.global.retcode
      else return (set_retcode env retcode)
    in
    body env 0

  and eval_for_loop env { head; commands } =
    match head with
    | WordsIn (name, args) ->
      eval_cmd env args
      >>= fun (env, argv) ->
      List.fold_left
        (fun env value ->
          env
          >>= fun env ->
          return
            (eval_atom_var_assign ~is_global:true (env, { name; subscript = "0" }, value))
          >>= fun env -> eval_any_cmds env commands)
        (return env)
        argv
    | ArithmTriple (before, cond, update) ->
      eval_arithm_expansion env before
      >>= fun (env, _) ->
      let rec body env retcode =
        eval_arithm_expansion env cond
        >>= fun (env, value) ->
        if value != 0
        then
          eval_any_cmds env commands
          >>= fun env ->
          let { retcode } = env.global in
          eval_arithm_expansion env update >>= fun (env, _) -> body env retcode
        else return (set_retcode env retcode)
      in
      body env 0

  and eval_loop env = function
    | WhileCompound while_loop -> eval_while_loop env while_loop
    | UntilCompound until_loop -> eval_until_loop env until_loop
    | ForCompound for_loop -> eval_for_loop env for_loop

  and eval_if_compound env = function
    | { condition; cons_cmds = cmds } :: tl ->
      eval_any_cmds env condition
      >>= fun env ->
      if env.global.retcode = 0 then eval_any_cmds env cmds else eval_if_compound env tl
    | [] -> return env

  and eval_compound env = function
    | Group _ -> failwith "Not implemented yet"
    | Loop loop -> eval_loop env loop
    | IfCompound if_compound -> eval_if_compound env if_compound
    | CaseIn _ -> failwith "Not implemented yet"
    | ArithmCompound arithm -> eval_arithm_compound env arithm

  and eval_any_cmd env = function
    | Simple (Operands ops) -> eval_operands env ops
    | Compound compound -> eval_compound env compound

  and eval_any_cmds env = function
    | hd :: tl -> eval_any_cmd env hd >>= fun env -> eval_any_cmds env tl
    | [] -> return env
  ;;

  let eval_func env { name; body } = return (set_fn env name body)

  let rec eval ?(env = default_session_env) = function
    | Command any_cmd :: tl -> eval_any_cmd env any_cmd >>= fun env -> eval ~env tl
    | Func f :: tl -> eval_func env f >>= fun env -> eval ~env tl
    | [] -> return env
  ;;
end

open Interpret (Result)

type 'a test_type =
  { ok : string -> 'a -> bool
  ; fail : string -> bool
  }

(* Tests is global variables are equal *)
let test_env_vars =
  let ok input expected =
    match Parser.parse input with
    | Ok script ->
      (match eval script with
       | Ok env when env.global.vars = expected -> true
       | Ok env ->
         pp_session_env Format.std_formatter env;
         false
       | Error e ->
         print_string e;
         false)
    | Error e ->
      Parser.pp_error Format.std_formatter e;
      false
  in
  let fail input =
    match Parser.parse input with
    | Ok script ->
      (match eval script with
       | Ok env ->
         pp_session_env Format.std_formatter env;
         false
       | Error _ -> true)
    | Error e ->
      Parser.pp_error Format.std_formatter e;
      false
  in
  { ok; fail }
;;

(* Test simple assignment *)

let%test _ =
  test_env_vars.ok
    {|a=10
    b=$a|}
    (StrMap.from_list
       [ "a", IndexedArray (IntMap.singleton 0 "10")
       ; "b", IndexedArray (IntMap.singleton 0 "10")
       ])
;;

(* Test if else compound *)

let%test _ =
  test_env_vars.ok
    {|if ((1>2));
    then
      a=10
    elif ((4<8)); then
      b=9
    else
      c=227
    fi|}
    (StrMap.from_list [ "b", IndexedArray (IntMap.singleton 0 "9") ])
;;

(* Test loops *)

let%test _ =
  test_env_vars.ok
    {|
  i=1
  until ((i>10));
  do
    i=$((i+1))
  done
|}
    (StrMap.singleton "i" (IndexedArray (IntMap.singleton 0 "11")))
;;

let%test _ =
  test_env_vars.ok
    {|
  i=1
  while ((i<10));
  do
    i=$((i+1))
  done
|}
    (StrMap.singleton "i" (IndexedArray (IntMap.singleton 0 "10")))
;;

let%test _ =
  test_env_vars.ok
    {|num=5
  fact=1

  for ((i=2;i<=num;i=i+1));
  do
    fact=$((fact * i))
  done|}
    (StrMap.from_list
       [ "num", IndexedArray (IntMap.singleton 0 "5")
       ; "fact", IndexedArray (IntMap.singleton 0 "120")
       ; "i", IndexedArray (IntMap.singleton 0 "6")
       ])
;;

let%test _ =
  test_env_vars.ok
    {|
    for e in some what;
    do
      echo=$e
    done|}
    (StrMap.from_list
       [ "e", IndexedArray (IntMap.singleton 0 "what")
       ; "echo", IndexedArray (IntMap.singleton 0 "what")
       ])
;;

(* Test functions *)

let%test _ =
  test_env_vars.ok
    {|function what () {
      a1=5
      a2=$2
    }
    b=10
    what some some2|}
    (StrMap.from_list
       [ "b", IndexedArray (IntMap.singleton 0 "10")
       ; "a1", IndexedArray (IntMap.singleton 0 "5")
       ; "a2", IndexedArray (IntMap.singleton 0 "some2")
       ])
;;

(* local variables in second fn are hidden for what fn *)
let%test _ =
  test_env_vars.ok
    {|function what () {
      c1=5
      a2=$1
    }
    function second () {
      a1=5
      l=$1
      what
      d=$1
    }
    second some|}
    (StrMap.from_list
       [ "a1", IndexedArray (IntMap.singleton 0 "5")
       ; "l", IndexedArray (IntMap.singleton 0 "some")
       ; "c1", IndexedArray (IntMap.singleton 0 "5")
       ; "a2", IndexedArray (IntMap.singleton 0 "")
       ; "d", IndexedArray (IntMap.singleton 0 "some")
       ])
;;

let%test _ =
  test_env_vars.ok
    {|
    function factorial () {
      num=$1
      fact=1

      for (( i=2; i <= num; i=i+1 ));
      do
        fact=$(( fact * i ))
      done
    }
    factorial 5
  |}
    (StrMap.from_list
       [ "num", IndexedArray (IntMap.singleton 0 "5")
       ; "fact", IndexedArray (IntMap.singleton 0 "120")
       ; "i", IndexedArray (IntMap.singleton 0 "6")
       ])
;;
