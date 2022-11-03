(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Utils
open Unix

module type MonadFail = sig
  include Base.Monad.Infix

  val return : 'a -> 'a t
  val fail : string -> 'a t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val ( *> ) : 'a t -> 'b -> 'b t
  val ( <* ) : 'a t -> 'b -> 'a t
end

module Result : MonadFail with type 'a t = ('a, string) result = struct
  type 'a t = ('a, string) result

  let return = Result.ok
  let fail = Result.error
  let ( >>= ) = Result.bind
  let ( >>| ) f g = f >>= fun x -> return (g x)
  let ( *> ) f g = f >>= fun _ -> return g
  let ( <* ) f g = f >>= fun e -> return g >>= fun _ -> return e

  let ( <|> ) f g =
    match f with
    | Ok _ -> f
    | Error _ -> g
  ;;
end

module Interpret (M : MonadFail) = struct
  open M

  (* simple variables are also indexed arrays *)
  type variable =
    | IndexedArray of string IntMap.t
    | AssocArray of string StrMap.t
  [@@deriving variants, show { with_path = false }]

  (* it is used when executing functions *)
  type local_variables = variable StrMap.t [@@deriving show { with_path = false }]

  (* std file descriptors
    (for writing more readable code)
  *)
  type std_fd =
    | StdIn
    | StdOut
    | StdErr

  let type_to_std_fd = function
    | StdIn -> 0
    | StdOut -> 1
    | StdErr -> 2
  ;;

  (* order is important *)
  let std_fd_types = [ StdIn; StdOut; StdErr ]

  (* function-resolver for default std descriptors *)
  let num_to_std_fd = function
    | 0 -> stdin
    | 1 -> stdout
    | 2 -> stderr
    | _ -> failwith "Cannot resolve fd"
  ;;

  (* List of standart fds *)
  let std_fds = List.map (fun x -> num_to_std_fd (type_to_std_fd x)) std_fd_types

  (* global environment *)
  type environment =
    { vars : variable StrMap.t
    ; functions : script StrMap.t
    ; retcode : int
    ; fds : file_descr IntMap.t [@printer fun _ _ -> ()]
    }
  [@@deriving show { with_path = false }]

  let default_global_env =
    { vars = StrMap.empty
    ; functions = StrMap.empty
    ; retcode = 1
    ; fds = IntMap.from_list std_fds
    }
  ;;

  type session_env =
    { local : local_variables
    ; global : environment
    }
  [@@deriving show { with_path = false }]

  let default_session_env = { local = StrMap.empty; global = default_global_env }
  let reset_local_env { global } = { local = StrMap.empty; global }
  let set_local_env env local = { env with local }
  let set_retcode env i = { env with global = { env.global with retcode = i } }

  let set_fd env key value =
    { env with global = { env.global with fds = IntMap.add key value env.global.fds } }
  ;;

  let set_std_fd env fd_type = set_fd env (type_to_std_fd fd_type)
  let get_std_fd { global = { fds } } fd_type = IntMap.find (type_to_std_fd fd_type) fds

  (* duplicate standard fds *)
  let dup_fds { global = { fds } } =
    IntMap.iter
      (fun n fd ->
        match n with
        | 0 | 1 | 2 -> dup2 fd (num_to_std_fd n)
        | _ -> ())
      fds
  ;;

  (* string value to variable with default subscript *)
  let str_to_var name = { name; subscript = "0" }

  (* int value to variable with default subscript *)
  let i_to_var i = str_to_var (string_of_int i)

  (* find variable in local env and global env *)
  let get_var_by_name { local; global } name =
    StrMap.find_opt name local
    |> function
    | None -> StrMap.find_opt name global.vars
    | v -> v
  ;;

  (* set variable either to local env, either to global env *)
  let set_var ~is_global { local; global } name v =
    if is_global
    then { local; global = { global with vars = StrMap.add name v global.vars } }
    else { local = StrMap.add name v local; global }
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
    | PositionalParam i -> (get_var_value env (i_to_var i)).as_str
    | VarNameExpansion name ->
      val_or_empty
        ~after:
          (function
           | IndexedArray v -> val_or_empty ~after:Fun.id (IntMap.find_opt 0 v)
           (* for associative arrays, the default value for the subscript is undefined *)
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
        eval_atom_var_assign ~is_global:true env (var, string_of_int value), value
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
    inner env arithm

  and eval_command_substitution env operands =
    let fd_in, fd_out = pipe () in
    let up_env = set_fd env 1 fd_out in
    eval_operands up_env operands
    >>| fun { global = { retcode } } ->
    close fd_out;
    let output = In_channel.input_all (in_channel_of_descr fd_in) in
    close fd_in;
    (* we're using env before evaluating operands and retcode from
       updated environment *)
    set_retcode env retcode, Base.String.rstrip output

  and eval_atom_string env = function
    | Text str -> return (env, str)
    | ParamExpansion param_exp -> return (env, eval_param_expansion env param_exp)
    | CommandSubstitution (Operands operands) -> eval_command_substitution env operands
    | ArithmExpansion arithm ->
      eval_arithm_expansion env arithm >>| fun (env, value) -> env, string_of_int value

  and eval_simple_string env = function
    | SingleQuotedString str -> return (env, str)
    | AtomString list | DoubleQuotedString list -> eval_concat env eval_atom_string list

  and eval_single_arg env single_arg = eval_concat env eval_simple_string single_arg

  and eval_multiple_args env =
    (* This is an interesting Bash brace expansion feature.*)

    (* I was expecting a question on the abstract syntax tree, specifically regarding
       VarNameExpansion ($name) and VarExpansion (${name} | ${name[subscript]})
       in type param_expansion.
       It seems, that we can use VarExpansion for both cases.
       But they have different behavior when we process brace expansion.
       Brace expansion is the only expansion (except for the tilde expansion) after
       processing which may increase the number of variable references.

       I will just show examples:
       ab=10 a=30
       echo $a{b,d,e}
       After brace expansion processing we will have:
       echo $ab $ad $ae
       We must then process varname expansion and output will be "10"

       But if we use VarExpansion:
       echo ${a}{b,d,e}
       After brace expansion processing we will have:
       ab=10 a=30
       echo ${a}b ${a}d ${a}e
       After processing this command we will have output "30"

       And in this function we process VarNameExpansion to support this bash feature.
    *)
    let rec process_atom_string_list = function
      | ParamExpansion (VarNameExpansion name) :: Text str :: tl ->
        let processed_expansion = ParamExpansion (VarNameExpansion (name ^ str)) in
        (* we must recursive start our list processing from already processed expansion
           because we can have situation like
           [$a; text1; text2]
           And we firstly process $a::text1 to $atext1, then we will process
           [$atext1, text2] to [$atext1text2]
        *)
        process_atom_string_list (processed_expansion :: tl)
      | hd :: tl -> hd :: process_atom_string_list tl
      | [] -> []
    in
    let rec process_single_arg = function
      | AtomString l1 :: AtomString l2 :: tl ->
        process_single_arg
          (AtomString (process_atom_string_list (List.concat [ l1; l2 ])) :: tl)
      | hd :: tl -> hd :: process_single_arg tl
      | [] -> []
    in
    let eval_arg env arg = eval_single_arg env (process_single_arg arg) in
    eval_elems env eval_arg

  and eval_arg env = function
    | SingleArg single_arg ->
      eval_single_arg env single_arg >>| fun (env, str) -> env, str :: []
    | MultipleArgs args -> eval_multiple_args env args

  and eval_atom_var_assign ~is_global env ({ name; subscript }, str) =
    (match get_var_by_name env name with
     | Some (IndexedArray arr) ->
       IndexedArray (IntMap.add (int_of_string subscript) str arr)
     | Some (AssocArray arr) -> AssocArray (StrMap.add subscript str arr)
     | None -> IndexedArray (IntMap.singleton (int_of_string subscript) str))
    |> set_var ~is_global env name

  (* update environment (process all types of variables) *)
  and set_env_var ?(is_global = true) env = function
    | AtomVariable { key; value } ->
      eval_single_arg env value
      >>| fun (env, str) -> eval_atom_var_assign ~is_global env (key, str)
    | IndexedArray { key = name; value = list } ->
      eval_elems env eval_arg list
      >>| fun (env, list) ->
      set_var ~is_global env name (IndexedArray (IntMap.from_list (List.concat list)))
    | AssocArray { key = name; value = list } ->
      eval_key_value_elems env eval_single_arg list
      >>| fun (env, list) ->
      set_var ~is_global env name (AssocArray (StrMap.from_list list))

  and eval_cmd env args =
    List.fold_left
      (fun acc arg ->
        acc >>= fun (env, strs) -> eval_arg env arg >>| fun (env, s) -> env, s :: strs)
      (return (env, []))
      args
    >>| fun (env, acc) -> env, List.concat (List.rev acc)

  (* save local env before executing f
     set saved local environment env after executing f *)
  and process_local_env ?(use_updated_env = true) env f =
    let { local } = env in
    let env = reset_local_env env in
    f env >>| fun up_env -> set_local_env (if use_updated_env then up_env else env) local

  and eval_exec_cmd env argv =
    let eval_exec env argv =
      dup_fds env;
      (* TODO: Add local variables to environment *)
      try execvpe (List.hd argv) (Array.of_list argv) (environment ()) with
      (* Todo: Fix this *)
      | Unix_error _ -> failwith "unix error"
    in
    let rec wait pid =
      try
        let _, status = waitpid [] pid in
        return
          (match status with
           | WEXITED retcode -> set_retcode env retcode
           (* the shell was terminated by the signal N *)
           | WSIGNALED n | WSTOPPED n -> set_retcode env (128 + n))
      with
      | Sys.Break ->
        (try kill pid Sys.sigint with
         | Unix_error (EACCES, _, _) ->
           print_endline "EACCES: cannot interrupt the current process");
        wait pid
    in
    match fork () with
    | 0 -> eval_exec env argv
    | pid ->
      let work () =
        Sys.catch_break true;
        wait pid
      in
      Fun.protect ~finally:(fun () -> Sys.catch_break false) work

  (* TODO: add invert return code *)
  and eval_atom_operand env { invert = _; env_vars; cmd } =
    let rec set_env_vars env = function
      | hd :: tl -> set_env_var env hd >>= fun env -> set_env_vars env tl
      | [] -> return env
    in
    (* [to_pos_arg i value] -> ({name="i+1", subscript="0"}, value) *)
    let to_pos_arg i value = i_to_var (i + 1), value in
    (* set positional arguments to local environment
       for command "echo word1 word2" arguments is [word1; word2].
       They will be transformed to positional arguments $1=word1; $2=word2.
       These arguments will be set in local environment
       *)
    let set_pos_args env tl =
      tl
      |> List.mapi to_pos_arg
      |> List.fold_left (eval_atom_var_assign ~is_global:false) env
    in
    let eval_function env body args =
      process_local_env env (fun env -> eval_impl ~env:(set_pos_args env args) body)
    in
    eval_cmd env cmd
    >>= fun (env, argv) ->
    match argv with
    | hd :: tl ->
      (match get_fn env hd with
       (* TODO: add script processing  *)
       | None -> eval_exec_cmd env argv
       | Some body -> eval_function env body tl)
    | [] -> set_env_vars env env_vars >>| fun env -> set_retcode env 0

  and eval_operand env =
    let or_and lhs rhs cmp =
      eval_operand env lhs
      >>= fun env -> if cmp env.global.retcode then eval_operand env rhs else return env
    in
    function
    | AtomOperand op -> eval_atom_operand env op
    | OrOperand (lhs, rhs) -> or_and lhs rhs (( = ) 0)
    | AndOperand (lhs, rhs) -> or_and lhs rhs (( != ) 0)

  and eval_pipes env operands =
    (*
       Before starting the explanation, I want to note that the "pipe" is the part
       of the command that stands between the vertical lines.
       "echo hello | cat" has two "pipes": "echo hello" and "cat"
       And each "pipe" in the above example has one operand.
       But "echo hello && echo some" has two operands "echo hello" and "echo some"

       Pipes evaluation has interesting feature:
          - only first pipe can update global environment variables,
          - other pipes cannot change global environment variables.
       For example, in the following command
          a=10 && echo $a | cat
       output is 10
       But in the following command
          c=28 && d=50 | a=10 && f=50 | echo $a
       output will be empty (and after this command we can't get var "a" value)

       Also, as you can see we can get variable values right in the commands!
       c=28 && echo $c | cat    --> 28
       c=28 && b=99 | echo $c   --> 28

       And, you can see that I used logical and in all examples above.
       Yes, it is necessary, because in each pipe only first n-1 operand can
       change variables value. Last operand -- cannot.
       For example:
       in command "c=28 && b=99 | echo $c " we cannot get b value.
    *)

    (* save fd in which we must write result *)
    let result_out = get_std_fd env StdOut in
    let fd_in, fd_out = pipe () in
    (* update env, set pipe's ouput as stdout *)
    eval_operand (set_std_fd env StdOut fd_out) (List.hd operands)
    >>= fun env ->
    close fd_out;
    (* we already processed first operand *)
    let operands = List.tl operands in
    (* eval_operand writes output in fd_out,
       then we must read from fd_in *)
    let env = set_std_fd env StdIn fd_in in
    (* set saved fd to environment stdout *)
    let env = set_std_fd env StdOut result_out in
    let rec eval_pipe env operand = function
      | hd :: tl ->
        let fd_in, fd_out = pipe () in
        eval_operand (set_std_fd env StdOut fd_out) operand
        >>= fun _ ->
        close (get_std_fd env StdIn);
        close fd_out;
        eval_pipe (set_std_fd env StdIn fd_in) hd tl
      | [] ->
        (* we don't need to open pipe for last operand
         this is why we consider this case (evaluating last operand)
         separately.*)
        eval_operand env operand
        >>| fun env ->
        close (get_std_fd env StdIn);
        env
    in
    eval_pipe env (List.hd operands) (List.tl operands)

  and eval_operands env = function
    | hd :: [] -> eval_operand env hd
    | [] -> failwith "After parsing at least 1 operand must be defined"
    | operands -> eval_pipes env operands

  and eval_arithm_compound env arithm =
    eval_arithm_expansion env arithm
    >>| fun (env, value) -> (if value != 0 then 0 else 1) |> set_retcode env

  and eval_until_loop env { condition; cons_cmds } =
    let rec body ?(retcode = 0) env =
      eval_any_cmds env condition
      >>= fun env ->
      if env.global.retcode != 0
      then eval_any_cmds env cons_cmds >>= fun env -> body env ~retcode:env.global.retcode
      else return (set_retcode env retcode)
    in
    body env

  (* equal to until_loop, but we use another comparator for retcode *)
  and eval_while_loop env { condition; cons_cmds } =
    let rec body ?(retcode = 0) env =
      eval_any_cmds env condition
      >>= fun env ->
      if env.global.retcode = 0
      then eval_any_cmds env cons_cmds >>= fun env -> body env ~retcode:env.global.retcode
      else return (set_retcode env retcode)
    in
    body env

  and eval_for_loop env { head; commands } =
    match head with
    | WordsIn (name, args) ->
      eval_cmd env args
      >>= fun (env, argv) ->
      List.fold_left
        (fun env value ->
          env
          >>= fun env ->
          return (eval_atom_var_assign ~is_global:true env (str_to_var name, value))
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
    | Simple (Operands operands) -> eval_operands env operands
    | Compound compound -> eval_compound env compound

  and eval_any_cmds env = function
    | hd :: tl -> eval_any_cmd env hd >>= fun env -> eval_any_cmds env tl
    | [] -> return env

  and eval_func env { name; body } = return (set_fn env name body)

  (* script evaluation, returns global and local environment.
    Needed only for tests, it makes no sense to give out information
    about the local environment *)
  and eval_impl ?(env = default_session_env) script =
    let rec eval_inner ~env = function
      | Command any_cmd :: tl ->
        eval_any_cmds env [ any_cmd ] >>= fun env -> eval_inner ~env tl
      | Func f :: tl -> eval_func env f >>= fun env -> eval_inner ~env tl
      | [] -> return env
    in
    eval_inner ~env script
  ;;

  let eval script = eval_impl script >>| fun { global } -> global
end

(** --------------- Tests --------------- **)

open Interpret (Result)

type 'a test_type =
  { ok :
      ?local_vars:variable StrMap.t
      -> ?global_vars:variable StrMap.t
      -> ?retcode:int
      -> name
      -> bool
  ; fail : string -> bool
  }

(* Tests local and global variables *)
let test =
  (* common logic for ok and fail tests:
     parse input optimistically and evaluate script  *)
  let test_inner input =
    match Parser.parse input with
    | Ok script -> eval_impl script
    | Error e ->
      Parser.pp_error Format.std_formatter e;
      failwith "Error while parsing test input (assuming the test gets the correct input)"
  in
  let ok ?(local_vars = StrMap.empty) ?(global_vars = StrMap.empty) ?(retcode = 0) input =
    match test_inner input with
    | Ok { local; global }
      when local_vars = local && global_vars = global.vars && retcode = global.retcode ->
      true
    | Ok env ->
      pp_session_env Format.std_formatter env;
      false
    | Error e ->
      print_string e;
      false
  in
  let fail input =
    match test_inner input with
    | Ok env ->
      pp_session_env Format.std_formatter env;
      false
    | Error _ -> true
  in
  { ok; fail }
;;

(* Test simple assignment *)

let%test _ =
  test.ok
    {|a=10
    b=$a|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "10")
         ; "b", IndexedArray (IntMap.singleton 0 "10")
         ])
;;

(* Test if else compound *)

let%test _ =
  test.ok
    {|if ((1>2));
    then
      a=10
    elif ((4<8)); then
      b=9
    else
      c=227
    fi|}
    ~global_vars:(StrMap.from_list [ "b", IndexedArray (IntMap.singleton 0 "9") ])
;;

(* Test loops *)

let%test _ =
  test.ok
    {|
  i=1
  until ((i>10));
  do
    i=$((i+1))
  done
|}
    ~global_vars:(StrMap.singleton "i" (IndexedArray (IntMap.singleton 0 "11")))
;;

let%test _ =
  test.ok
    {|
  i=1
  while ((i<10));
  do
    i=$((i+1))
  done
|}
    ~global_vars:(StrMap.singleton "i" (IndexedArray (IntMap.singleton 0 "10")))
;;

let%test _ =
  test.ok
    {|num=5
  fact=1

  for ((i=2;i<=num;i=i+1));
  do
    fact=$((fact * i))
  done|}
    ~global_vars:
      (StrMap.from_list
         [ "num", IndexedArray (IntMap.singleton 0 "5")
         ; "fact", IndexedArray (IntMap.singleton 0 "120")
         ; "i", IndexedArray (IntMap.singleton 0 "6")
         ])
;;

let%test _ =
  test.ok
    {|
    for e in some what;
    do
      echo=$e
    done|}
    ~global_vars:
      (StrMap.from_list
         [ "e", IndexedArray (IntMap.singleton 0 "what")
         ; "echo", IndexedArray (IntMap.singleton 0 "what")
         ])
;;

(* Test command substitution *)

let%test _ =
  test.ok
    {|a=$(echo hello)|}
    ~global_vars:(StrMap.from_list [ "a", IndexedArray (IntMap.singleton 0 "hello") ])
;;

let%test _ =
  test.ok
    {|a=$(echo $(echo hello2))|}
    ~global_vars:(StrMap.from_list [ "a", IndexedArray (IntMap.singleton 0 "hello2") ])
;;

(* Test functions *)

let%test _ =
  test.ok
    {|function what () {
      a1=5
      a2=$2
    }
    b=10
    what some some2|}
    ~global_vars:
      (StrMap.from_list
         [ "b", IndexedArray (IntMap.singleton 0 "10")
         ; "a1", IndexedArray (IntMap.singleton 0 "5")
         ; "a2", IndexedArray (IntMap.singleton 0 "some2")
         ])
;;

(* local variables in second fn are hidden for what fn *)
let%test _ =
  test.ok
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
    ~global_vars:
      (StrMap.from_list
         [ "a1", IndexedArray (IntMap.singleton 0 "5")
         ; "l", IndexedArray (IntMap.singleton 0 "some")
         ; "c1", IndexedArray (IntMap.singleton 0 "5")
         ; "a2", IndexedArray (IntMap.singleton 0 "")
         ; "d", IndexedArray (IntMap.singleton 0 "some")
         ])
;;

let%test _ =
  test.ok
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
    ~global_vars:
      (StrMap.from_list
         [ "num", IndexedArray (IntMap.singleton 0 "5")
         ; "fact", IndexedArray (IntMap.singleton 0 "120")
         ; "i", IndexedArray (IntMap.singleton 0 "6")
         ])
;;

let%test _ =
  test.ok
    {|
    function factorial () {
      if (($1=1))
      then
        fact=1
      else
        factorial $(($1 - 1))
        fact=$((fact * $1))
      fi
    }
    factorial 5
  |}
    ~global_vars:(StrMap.from_list [ "fact", IndexedArray (IntMap.singleton 0 "120") ])
;;

let%test _ =
  test.ok
    {|
    function factorial () {
      if (( $1 <= 1 )); then
          echo 1
      else
          last=$(factorial $(( $1 - 1 )))
          echo $(( $1 * last ))
      fi
    }

    a=$(factorial 5)|}
    ~global_vars:(StrMap.from_list [ "a", IndexedArray (IntMap.singleton 0 "120") ])
;;

let%test _ =
  test.ok
    {|
    function what () {
      b=$(echo hello)
    }

    a=$(what)|}
    ~global_vars:(StrMap.from_list [ "a", IndexedArray (IntMap.singleton 0 "") ])
;;

let%test _ =
  test.ok
    {|
    function what () {
      d=$1
      b=`echo $((a=10))`
      e=$1
    }
    l=22
    what some|}
    ~global_vars:
      (StrMap.from_list
         [ "l", IndexedArray (IntMap.singleton 0 "22")
         ; "d", IndexedArray (IntMap.singleton 0 "some")
         ; "b", IndexedArray (IntMap.singleton 0 "10")
         ; "e", IndexedArray (IntMap.singleton 0 "some")
         ])
;;

let%test _ =
  test.ok
    {|function what () {
        a1=5
        function what2 () {
          a3=9
        }
        what2
      }
      what
    |}
    ~global_vars:
      (StrMap.from_list
         [ "a1", IndexedArray (IntMap.singleton 0 "5")
         ; "a3", IndexedArray (IntMap.singleton 0 "9")
         ])
;;

let%test _ =
  test.ok
    {|function what () {
        a1=5
        function what2 () {
          a3=9
        }
      }
      what
      what2
    |}
    ~global_vars:
      (StrMap.from_list
         [ "a1", IndexedArray (IntMap.singleton 0 "5")
         ; "a3", IndexedArray (IntMap.singleton 0 "9")
         ])
;;

let%test _ =
  test.ok
    {|function what () {
        a1=$1
        function what2 () {
          a3=$1
        }
        what2
      }
      what some
    |}
    ~global_vars:
      (StrMap.from_list
         [ "a1", IndexedArray (IntMap.singleton 0 "some")
         ; "a3", IndexedArray (IntMap.singleton 0 "")
         ])
;;

(* Tests for multiple args *)

let%test _ =
  test.ok
    {|a=`echo "some"'some'{b,d,e}d`|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "somesomebd somesomedd somesomeed") ])
    ~retcode:0
;;

let%test _ =
  test.ok
    {|
    ab=10
    ad=28
    a=`echo $a{b,d,e}`|}
    ~global_vars:
      (StrMap.from_list
         [ "ab", IndexedArray (IntMap.singleton 0 "10")
         ; "ad", IndexedArray (IntMap.singleton 0 "28")
         ; "a", IndexedArray (IntMap.singleton 0 "10 28")
         ])
;;

(* Tests for simple pipes  *)

let%test _ =
  test.ok
    {|a=$(echo hello|cat)|}
    ~global_vars:(StrMap.from_list [ "a", IndexedArray (IntMap.singleton 0 "hello") ])
    ~retcode:0
;;

let%test _ =
  test.ok
    {|a=hello |echo why|cat|}
    ~global_vars:(StrMap.from_list [ "a", IndexedArray (IntMap.singleton 0 "hello") ])
;;

let%test _ =
  test.ok
    {|a=`echo why|cat|echo why2|cat`|}
    ~global_vars:(StrMap.from_list [ "a", IndexedArray (IntMap.singleton 0 "why2") ])
;;
