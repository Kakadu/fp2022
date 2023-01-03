(** Copyright 2021-2022, Chizhov Anton *)

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

  (* global environment *)
  type environment =
    { vars : variable StrMap.t
    ; functions : script StrMap.t
    ; retcode : int
    ; fds : file_descr IntMap.t [@printer fun _ _ -> ()]
    }
  [@@deriving show { with_path = false }]

  let default_env =
    { vars = StrMap.empty
    ; functions = StrMap.empty
    ; retcode = 0
    ; fds = IntMap.from_list std_fds
    }
  ;;

  type session_env =
    { local : local_variables
    ; global : environment
    }
  [@@deriving show { with_path = false }]

  let default_session_env = { local = StrMap.empty; global = default_env }
  let reset_local_env { global } = { local = StrMap.empty; global }
  let set_local_env env local = { env with local }
  let set_retcode env retcode = { env with global = { env.global with retcode } }

  let set_fd env key value =
    { env with global = { env.global with fds = IntMap.add key value env.global.fds } }
  ;;

  let set_fds env fds = { env with global = { env.global with fds } }
  let set_std_fd env fd_type = set_fd env (fd_to_int fd_type)
  let get_fd_opt { global = { fds } } fd_num = IntMap.find_opt fd_num fds

  let get_std_fd, get_std_fd_opt =
    let get_fd find { global = { fds } } fd_type = find (fd_to_int fd_type) fds in
    get_fd IntMap.find, get_fd IntMap.find_opt
  ;;

  (* duplicate standard fds *)
  let dup_fds { global = { fds } } =
    IntMap.iter
      (fun n fd ->
        match n with
        | 0 -> dup2 fd Unix.stdin
        | 1 -> dup2 fd Unix.stdout
        | 2 -> dup2 fd Unix.stderr
        | _ -> ())
      fds
  ;;

  let try_write fd s =
    let len = String.length s in
    try write_substring fd s 0 len = len with
    | Unix_error (Unix.EBADF, "write", "") -> false
  ;;

  let print ?(std_fd = StdErr) env data =
    match get_std_fd_opt env std_fd with
    | Some stderr when try_write stderr (data ^ "\n") -> ()
    | _ -> ()
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

  (* a=5 -> a=5 *)
  (* a=(1 2 3) -> a=(1 2 3) *)
  (* a=(key=value key2=value2) -> a=(key=valye key2=value) *)
  let variable_to_string = function
    | IndexedArray array ->
      IntMap.fold (fun _ value acc -> value :: acc) array []
      |> (function
      | s :: [] -> s
      | s -> String.concat " " (List.rev s) |> Printf.sprintf "(%s)")
    | AssocArray array ->
      StrMap.fold (fun key value acc -> Printf.sprintf "%s=%s" key value :: acc) array []
      |> List.rev
      |> String.concat " "
      |> Printf.sprintf "(%s)"
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

  let rec eval_substring env { name; offset; length = len } =
    return (get_var_value env name).as_str
    >>= fun s ->
    eval_arithm_expansion env offset
    >>= fun (env, offset) ->
    (match len with
     | Some e -> eval_arithm_expansion env e >>| fun (env, n) -> env, Some n
     | None -> return (env, None))
    >>= fun (env, len) ->
    let open String in
    let offset =
      match offset with
      | value when value < 0 -> 0
      | value when value < length s -> offset
      | _ -> length s
    in
    let len =
      match len with
      | Some len when len >= 0 -> Base.min len (length s - offset)
      | Some len -> length s + len - offset
      | None -> length s - offset
    in
    if len >= 0
    then return (env, sub s offset len)
    else if length s = 0
    then return (env, "")
    else fail "substring expression < 0"

  and eval_substr_removal env { name; pattern; from; min_or_max } =
    let open Re in
    (get_var_value env name).as_str
    |> function
    | "" -> ""
    | s ->
      let f g =
        (match from with
         | FromBegin -> Group.start g 0 = 0
         | FromEnd -> Group.stop g 0 = String.length s)
        |> function
        | true -> ""
        | false -> Group.get g 0
      in
      let size = if min_or_max = Min then shortest else longest in
      let re = compile (size (Glob.glob pattern)) in
      replace ~all:false re ~f s

  and eval_substitute env { name; pattern; by; subst_type } =
    let open Re in
    (get_var_value env name).as_str
    |> function
    | "" -> by
    | s ->
      let f g =
        (match subst_type with
         | One | All -> true
         | First -> Group.start g 0 = 0
         | Last -> Group.stop g 0 = String.length s)
        |> function
        | true -> by
        | false -> Group.get g 0
      in
      let re = compile (longest (Glob.glob pattern)) in
      let all = subst_type = All || subst_type = Last in
      Re.replace ~all re ~f s

  and eval_param_expansion env = function
    | PositionalParam i -> return (env, (get_var_value env (i_to_var i)).as_str)
    | VarNameExpansion name ->
      return
        ( env
        , val_or_empty
            ~after:
              (function
               | IndexedArray v -> val_or_empty ~after:Fun.id (IntMap.find_opt 0 v)
               (* for associative arrays, the default value for the subscript is undefined *)
               | AssocArray _ -> "")
            (get_var_by_name env name) )
    | VarExpansion var -> return (env, (get_var_value env var).as_str)
    | Length var ->
      return (env, string_of_int (String.length (get_var_value env var).as_str))
    | Substring s -> eval_substring env s
    | SubstrRemoval s -> return (env, eval_substr_removal env s)
    | Substitute s -> return (env, eval_substitute env s)

  and eval_arithm_expansion env arithm =
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
    let up_env = set_std_fd env StdOut fd_out in
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
    | ParamExpansion param_exp -> eval_param_expansion env param_exp
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
       After processing this command we will have output "30b 30d 30e"

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

  and eval_script_file env path argv =
    let read_from in_ch =
      try Some (really_input_string in_ch (in_channel_length in_ch)) with
      | Sys_error _ -> None
      | End_of_file -> None
    in
    let set_pos_args env args =
      args
      |> List.mapi (fun i value -> i_to_var i, value)
      |> List.fold_left (eval_atom_var_assign ~is_global:false) env
    in
    match read_from (open_in path) with
    | None -> fail (Printf.sprintf "Cannot read script %s." path)
    | Some script ->
      (match Parser.parse script with
       | Ok script ->
         let env =
           { default_session_env with global = { default_env with fds = env.global.fds } }
         in
         eval_impl ~env:(set_pos_args env argv) script
         >>| fun { global = { retcode } } -> exit retcode
       | Error _ -> fail "Cannot parse script")

  and eval_exec_cmd env argv =
    let set_pos_args env args =
      args
      |> List.mapi (fun i value -> i_to_var i, value)
      |> List.fold_left (eval_atom_var_assign ~is_global:false) env
    in
    let environment env =
      StrMap.fold
        (fun name var acc -> String.concat "=" [ name; variable_to_string var ] :: acc)
        env.local
        []
      |> Array.of_list
      |> Array.append (environment ())
    in
    let eval_exec env argv =
      dup_fds env;
      try execvpe (List.hd argv) (Array.of_list argv) (environment env) with
      | Unix_error (ENOENT, _, _) -> exit 127
      | Unix_error (ENOEXEC, _, _) | Unix_error (EUNKNOWNERR 26, _, _) ->
        print env (Printf.sprintf "bash '%s'. Permission denied" (List.hd argv));
        exit 126
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
    | 0 ->
      let resolve_script_name name =
        if Sys.file_exists name && not (Sys.is_directory name)
        then (
          match name with
          | path when Filename.dirname path = "." -> Some (Filename.basename path)
          | path when Filename.dirname path = "" -> Some path
          | _ -> None)
        else None
      in
      (match resolve_script_name (List.hd argv) with
       | Some path -> eval_script_file (set_pos_args env argv) path argv
       | None -> eval_exec env argv)
    | pid ->
      let work () =
        Sys.catch_break true;
        wait pid
      in
      Fun.protect ~finally:(fun () -> Sys.catch_break false) work

  and eval_redir env =
    (* -rw-r----- *)
    let permissions = 0o640 in
    function
    | RedirInput (fd, arg) ->
      eval_single_arg env arg
      >>= fun (_, file) ->
      if Sys.file_exists file
      then return (set_fd env fd (openfile file [ O_RDONLY ] permissions))
      else fail (Printf.sprintf "%s: No such file or directory" file)
    | RedirOutput (fd, arg) ->
      eval_single_arg env arg
      >>| fun (_, file) -> set_fd env fd (openfile file [ O_CREAT; O_WRONLY ] permissions)
    | AppendOutput (fd, arg) ->
      eval_single_arg env arg
      >>| fun (_, file) ->
      set_fd env fd (openfile file [ O_CREAT; O_WRONLY; O_APPEND ] permissions)
    | DupInput (fd_num, arg) | DupOutput (fd_num, arg) ->
      eval_single_arg env arg
      >>= fun (_, s) ->
      (match int_of_string_opt s with
       | Some i ->
         (match get_fd_opt env i with
          | Some fd -> return (set_fd env fd_num fd)
          | None -> fail (string_of_int i ^ ": Bad file descriptor"))
       | None -> fail (s ^ ": Ambiguous redirect"))

  and eval_redirs env =
    List.fold_left (fun env redir -> env >>= fun env -> eval_redir env redir) (return env)

  and eval_atom_operand ~is_first env { invert; env_vars; cmd; redirs } =
    let process_invert retcode =
      match invert with
      | true -> if retcode = 0 then 1 else 0
      | false -> retcode
    in
    let rec set_env_vars ?(is_global = true) env = function
      | hd :: tl ->
        set_env_var ~is_global env hd >>= fun env -> set_env_vars ~is_global env tl
      | [] -> return env
    in
    (* [to_pos_arg i value] -> ({name="i+1", subscript="0"}, value) *)
    let to_pos_arg i value = i_to_var (i + 1), value in
    (* set positional arguments to local environment
       for command "echo word1 word2" arguments is [word1; word2].
       They will be transformed to positional arguments $1=word1; $2=word2.
       These arguments will be set in local environment
       *)
    let set_pos_args env args =
      args
      |> List.mapi to_pos_arg
      |> List.fold_left (eval_atom_var_assign ~is_global:false) env
    in
    let eval_function env env_vars body args =
      process_local_env env (fun env ->
        set_env_vars ~is_global:false env env_vars
        >>= fun env -> eval_impl ~env:(set_pos_args env args) body)
    in
    let { fds } = env.global in
    eval_redirs env redirs
    >>= fun env ->
    eval_cmd env cmd
    >>= fun (env, argv) ->
    (match argv with
     | hd :: tl ->
       (match get_fn env hd with
        | None ->
          process_local_env env (fun env ->
            set_env_vars ~is_global:false env env_vars
            >>= fun env -> eval_exec_cmd env argv)
        | Some body -> eval_function env env_vars body tl)
     | [] ->
       let temp_env = env in
       set_env_vars env env_vars
       >>| fun env -> set_retcode (if is_first then env else temp_env) 0)
    >>| fun env -> set_fds (set_retcode env (process_invert env.global.retcode)) fds

  (* ~is_first -- is first pipe
     ~can_be_last_operand -- can be last operand *)
  and eval_operand ~is_first ?(can_be_last_operand = false) env =
    let or_and lhs rhs cmp =
      eval_operand ~is_first env lhs
      >>= fun env ->
      if cmp env.global.retcode
      then eval_operand ~is_first ~can_be_last_operand env rhs
      else return env
    in
    function
    | AtomOperand op ->
      eval_atom_operand ~is_first:(is_first && not can_be_last_operand) env op
    | CompoundOperand c -> eval_compound env c
    | OrOperand (lhs, rhs) -> or_and lhs rhs (( != ) 0)
    | AndOperand (lhs, rhs) -> or_and lhs rhs (( = ) 0)

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
    eval_operand
      ~can_be_last_operand:true
      ~is_first:true
      (set_std_fd env StdOut fd_out)
      (List.hd operands)
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
        eval_operand
          ~can_be_last_operand:true
          ~is_first:false
          (set_std_fd env StdOut fd_out)
          operand
        >>= fun _ ->
        close (get_std_fd env StdIn);
        close fd_out;
        eval_pipe (set_std_fd env StdIn fd_in) hd tl
      | [] ->
        (* we don't need to open pipe for last operand
         this is why we consider this case (evaluating last operand)
         separately.*)
        eval_operand ~is_first:false env operand
        >>| fun env ->
        close (get_std_fd env StdIn);
        env
    in
    eval_pipe env (List.hd operands) (List.tl operands)

  and eval_operands env = function
    | hd :: [] -> eval_operand ~is_first:true env hd
    | [] -> fail "After parsing at least 1 operand must be defined"
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

  and eval_case_in env { by; cases } =
    let open Re in
    eval_single_arg env by
    >>= fun (env, by) ->
    let rec eval_item env = function
      | (s_args, any_cmd) :: tl ->
        eval_elems env eval_single_arg s_args
        >>= fun (env, patterns) ->
        List.filter (fun p -> execp (compile (Glob.glob ~anchored:true p)) by) patterns
        |> (function
        | [] -> eval_item env tl
        | _ -> eval_any_cmd env any_cmd)
      | [] -> return (set_retcode env 0)
    in
    eval_item env cases

  and eval_compound env = function
    | Group _ ->
      fail
        {|Not supported (the assignment states that support Grouping Commands is not needed)|}
    | Loop loop -> eval_loop env loop
    | IfCompound if_compound -> eval_if_compound env if_compound
    | CaseIn case_in -> eval_case_in env case_in
    | ArithmCompound arithm -> eval_arithm_compound env arithm

  and eval_any_cmd env = function
    | Simple (Operands operands) ->
      eval_operands env operands >>| fun n_env -> set_fds n_env env.global.fds
    | Compound (compound, redirs) ->
      eval_redirs env redirs >>= fun env -> eval_compound env compound

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

  let eval ?(env = default_env) script =
    eval_impl ~env:{ default_session_env with global = env } script
    >>| fun { global } -> global
  ;;
end

(** --------------- Tests --------------- **)

open Interpret (Result)

(* Tests local and global variables *)
let test_ok, test_fail =
  (* common logic for ok and fail tests:
     parse input optimistically and evaluate script  *)
  let test_inner input =
    match Parser.parse input with
    | Ok script -> eval_impl script
    | Error e ->
      Parser.pp_error Format.std_formatter e;
      Result.fail
        "Error while parsing test input (assuming the test gets the correct input)"
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
  ok, fail
;;

(* Test simple assignment *)

let%test _ =
  test_ok
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
  test_ok
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
  test_ok
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
  test_ok
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
  test_ok
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
  test_ok
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
  test_ok
    {|a=$(echo hello)|}
    ~global_vars:(StrMap.from_list [ "a", IndexedArray (IntMap.singleton 0 "hello") ])
;;

let%test _ =
  test_ok
    {|a=$(echo $(echo hello2))|}
    ~global_vars:(StrMap.from_list [ "a", IndexedArray (IntMap.singleton 0 "hello2") ])
;;

(* Test functions *)

let%test _ =
  test_ok
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
  test_ok
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
  test_ok
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
  test_ok
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
  test_ok
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
  test_ok
    {|
    function what () {
      b=$(echo hello)
    }

    a=$(what)|}
    ~global_vars:(StrMap.from_list [ "a", IndexedArray (IntMap.singleton 0 "") ])
;;

let%test _ =
  test_ok
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
  test_ok
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
  test_ok
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
  test_ok
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
  test_ok
    {|a=`echo "some"'some'{b,d,e}d`|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "somesomebd somesomedd somesomeed") ])
    ~retcode:0
;;

let%test _ =
  test_ok
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

let%test _ =
  test_ok
    {|
    ab=10 a=30
    d=`echo ${a}{a,b,c}`|}
    ~global_vars:
      (StrMap.from_list
         [ "ab", IndexedArray (IntMap.singleton 0 "10")
         ; "a", IndexedArray (IntMap.singleton 0 "30")
         ; "d", IndexedArray (IntMap.singleton 0 "30a 30b 30c")
         ])
;;

(* Tests for simple pipes  *)

let%test _ =
  test_ok
    {|a=$(echo hello|cat)|}
    ~global_vars:(StrMap.from_list [ "a", IndexedArray (IntMap.singleton 0 "hello") ])
    ~retcode:0
;;

let%test _ =
  test_ok
    {|f=`a=hello |echo why|cat`|}
    ~global_vars:(StrMap.from_list [ "f", IndexedArray (IntMap.singleton 0 "why") ])
;;

let%test _ =
  test_ok
    {|a=`echo why|cat|echo why2|cat`|}
    ~global_vars:(StrMap.from_list [ "a", IndexedArray (IntMap.singleton 0 "why2") ])
;;

(* Tests for param expansion *)

(* Test length *)
let%test _ =
  test_ok
    {|a=echo
    b=${#a[0]}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "echo")
         ; "b", IndexedArray (IntMap.singleton 0 "4")
         ])
;;

let%test _ =
  test_ok
    {|a=echo
    b=${#a[1]}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "echo")
         ; "b", IndexedArray (IntMap.singleton 0 "0")
         ])
;;

let%test _ =
  test_ok
    {|a=(key=value)
    b=${#a[key]}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", AssocArray (StrMap.singleton "key" "value")
         ; "b", IndexedArray (IntMap.singleton 0 "5")
         ])
;;

(* Test substring *)
let%test _ =
  test_ok
    {|a=some
    b=${a:0:3}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "some")
         ; "b", IndexedArray (IntMap.singleton 0 "som")
         ])
;;

let%test _ =
  test_ok
    {|a=some
    b=${a:0:-1}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "some")
         ; "b", IndexedArray (IntMap.singleton 0 "som")
         ])
;;

let%test _ =
  test_ok
    {|a=some
    b=${a:0:-4}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "some")
         ; "b", IndexedArray (IntMap.singleton 0 "")
         ])
;;

let%test _ =
  test_ok
    {|a=some
    b=${a:2:-1}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "some")
         ; "b", IndexedArray (IntMap.singleton 0 "m")
         ])
;;

let%test _ =
  test_ok
    {|a=some
    b=${a:-3}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "some")
         ; "b", IndexedArray (IntMap.singleton 0 "some")
         ])
;;

let%test _ =
  test_ok
    {|a=some
    b=${a:4}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "some")
         ; "b", IndexedArray (IntMap.singleton 0 "")
         ])
;;

let%test _ =
  test_ok
    {|a=some
    b=${a:5}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "some")
         ; "b", IndexedArray (IntMap.singleton 0 "")
         ])
;;

let%test _ =
  test_ok
    {|a=some
    b=${a:0:1}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "some")
         ; "b", IndexedArray (IntMap.singleton 0 "s")
         ])
;;

let%test _ =
  test_ok
    {|a=some
    b=${a:0:-3}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "some")
         ; "b", IndexedArray (IntMap.singleton 0 "s")
         ])
;;

let%test _ =
  test_ok
    {|a=some
    b=${a:0:-4}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "some")
         ; "b", IndexedArray (IntMap.singleton 0 "")
         ])
;;

let%test _ =
  test_ok
    {|a=some
    b=${a:0:200+500}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "some")
         ; "b", IndexedArray (IntMap.singleton 0 "some")
         ])
;;

let%test _ =
  test_ok
    {|b=${a:0:7}|}
    ~global_vars:(StrMap.from_list [ "b", IndexedArray (IntMap.singleton 0 "") ])
;;

(* Tests for substring removal *)
let%test _ =
  test_ok
    {|a=abcde
    b=${a#*}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "abcde")
         ; "b", IndexedArray (IntMap.singleton 0 "abcde")
         ])
;;

let%test _ =
  test_ok
    {|a=abcde
    b=${a#a}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "abcde")
         ; "b", IndexedArray (IntMap.singleton 0 "bcde")
         ])
;;

let%test _ =
  test_ok
    {|a=abcde
    b=${a#abcd}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "abcde")
         ; "b", IndexedArray (IntMap.singleton 0 "e")
         ])
;;

let%test _ =
  test_ok
    {|a=abcde
    b=${a#?}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "abcde")
         ; "b", IndexedArray (IntMap.singleton 0 "bcde")
         ])
;;

let%test _ =
  test_ok
    {|a=abcde
    b=${a#[0-9a-Z]}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "abcde")
         ; "b", IndexedArray (IntMap.singleton 0 "bcde")
         ])
;;

let%test _ =
  test_ok
    {|a=abcde
    b=${a#[a-Z0-9]b[c-c]}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "abcde")
         ; "b", IndexedArray (IntMap.singleton 0 "de")
         ])
;;

let%test _ =
  test_ok
    {|a=abcde
    b=${a##*}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "abcde")
         ; "b", IndexedArray (IntMap.singleton 0 "")
         ])
;;

let%test _ =
  test_ok
    {|a=abcde
    b=${a##[a-c]?}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "abcde")
         ; "b", IndexedArray (IntMap.singleton 0 "cde")
         ])
;;

let%test _ =
  test_ok
    {|a=abcde
    b=${a%e}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "abcde")
         ; "b", IndexedArray (IntMap.singleton 0 "abcd")
         ])
;;

let%test _ =
  test_ok
    {|a=abcde
    b=${a%de}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "abcde")
         ; "b", IndexedArray (IntMap.singleton 0 "abc")
         ])
;;

let%test _ =
  test_ok
    {|a=abcde
    b=${a%%*}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "abcde")
         ; "b", IndexedArray (IntMap.singleton 0 "")
         ])
;;

let%test _ =
  test_ok
    {|a=abcde
    b=${a%%*}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "abcde")
         ; "b", IndexedArray (IntMap.singleton 0 "")
         ])
;;

(* Test substitution *)

let%test _ =
  test_ok
    {|a=abcde
    b=${a/ab}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "abcde")
         ; "b", IndexedArray (IntMap.singleton 0 "cde")
         ])
;;

let%test _ =
  test_ok
    {|a=abcde
    b=${a/ab}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "abcde")
         ; "b", IndexedArray (IntMap.singleton 0 "cde")
         ])
;;

let%test _ =
  test_ok
    {|a=abcde
    b=${a/bc/abcd}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "abcde")
         ; "b", IndexedArray (IntMap.singleton 0 "aabcdde")
         ])
;;

let%test _ =
  test_ok
    {|a=abcde
    b=${a/bc/5798}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "abcde")
         ; "b", IndexedArray (IntMap.singleton 0 "a5798de")
         ])
;;

let%test _ =
  test_ok
    {|a=kawhatdu
    b=${a/what/ka}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "kawhatdu")
         ; "b", IndexedArray (IntMap.singleton 0 "kakadu")
         ])
;;

let%test _ =
  test_ok
    {|a=kawhatdu
    b=${a/*/ka}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "kawhatdu")
         ; "b", IndexedArray (IntMap.singleton 0 "ka")
         ])
;;

let%test _ =
  test_ok
    {|a=kawhatdu
    b=${a/****/ka}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "kawhatdu")
         ; "b", IndexedArray (IntMap.singleton 0 "ka")
         ])
;;

let%test _ =
  test_ok
    {|a=`echo`
    b=${a/*/ka}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "")
         ; "b", IndexedArray (IntMap.singleton 0 "ka")
         ])
;;

let%test _ =
  test_ok
    {|a=`echo`
    b=${a/?/ka}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "")
         ; "b", IndexedArray (IntMap.singleton 0 "ka")
         ])
;;

let%test _ =
  test_ok
    {|a=echo
    b=${a//*/notecho}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "echo")
         ; "b", IndexedArray (IntMap.singleton 0 "notecho")
         ])
;;

let%test _ =
  test_ok
    {|a=echo
    b=${a//?/ab}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "echo")
         ; "b", IndexedArray (IntMap.singleton 0 "abababab")
         ])
;;

let%test _ =
  test_ok
    {|a=kkdu
    b=${a//k/ka}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "kkdu")
         ; "b", IndexedArray (IntMap.singleton 0 "kakadu")
         ])
;;

let%test _ =
  test_ok
    {|a=kkdu
    b=${a//k}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "kkdu")
         ; "b", IndexedArray (IntMap.singleton 0 "du")
         ])
;;

let%test _ =
  test_ok
    {|a=kkdu
    b=${a/#k/so}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "kkdu")
         ; "b", IndexedArray (IntMap.singleton 0 "sokdu")
         ])
;;

let%test _ =
  test_ok
    {|a=kkdu
    b=${a/%kdu/so}|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "kkdu")
         ; "b", IndexedArray (IntMap.singleton 0 "kso")
         ])
;;

(* Test case in *)

let%test _ =
  test_ok
    {|case ${LO} in
      lo|fo) a="four";;
      *|ro) b=hello;;
      esac|}
    ~global_vars:(StrMap.from_list [ "b", IndexedArray (IntMap.singleton 0 "hello") ])
;;

let%test _ =
  test_ok
    {|
    LO=lo
    case ${LO} in
      lo|fo) a="four";;
      *|ro) b=hello;;
      esac|}
    ~global_vars:
      (StrMap.from_list
         [ "LO", IndexedArray (IntMap.singleton 0 "lo")
         ; "a", IndexedArray (IntMap.singleton 0 "four")
         ])
;;

let%test _ =
  test_ok
    {|
    LO=foo
    case ${LO} in
      lo|fo*) a="four";;
      *|ro) b=hello;;
      esac|}
    ~global_vars:
      (StrMap.from_list
         [ "LO", IndexedArray (IntMap.singleton 0 "foo")
         ; "a", IndexedArray (IntMap.singleton 0 "four")
         ])
;;

let%test _ =
  test_ok
    {|
    LO='it works!'
    case ${LO} in
      lo|'it works!') a="four";;
      *|ro) b=hello;;
      esac|}
    ~global_vars:
      (StrMap.from_list
         [ "LO", IndexedArray (IntMap.singleton 0 "it works!")
         ; "a", IndexedArray (IntMap.singleton 0 "four")
         ])
;;

(* Test error handler *)

let%test _ = test_ok {|echo2 hello|} ~retcode:127
let%test _ = test_ok {|! echo2 hello|} ~retcode:0

(* Set local variables to function *)

let%test _ =
  test_ok
    {|function what() {
      b=`echo $some`
    }
    some=10
    what|}
    ~global_vars:
      (StrMap.from_list
         [ "some", IndexedArray (IntMap.singleton 0 "10")
         ; "b", IndexedArray (IntMap.singleton 0 "10")
         ])
;;

(* set some=10 to function local variables, after function executing they will be removed *)
let%test _ =
  test_ok
    {|function what() {
      b=`echo $some`
    }
    some=10 what|}
    ~global_vars:(StrMap.from_list [ "b", IndexedArray (IntMap.singleton 0 "10") ])
;;

(* let%test _ = test_ok {|kak=10 printenv | wc|} *)

(* let%test _ =
  test_ok {|how=10 some=(5 5) done=(key=5) done2=(key=5 key2=19) printenv | wc|}
;; *)

let%test _ =
  test_ok
    {|echo echo dshfjklhjlkh >scr.sh
  chmod +x scr.sh
  a=`./scr.sh`
 |}
    ~global_vars:
      (StrMap.from_list [ "a", IndexedArray (IntMap.singleton 0 "dshfjklhjlkh") ])
;;

let%test _ =
  test_ok
    {|a=`echo hello2 >hello2.txt|echo hello3 > some44.txt | echo some`
  b=`cat hello2.txt`
  c=`cat some44.txt`|}
    ~global_vars:
      (StrMap.from_list
         [ "a", IndexedArray (IntMap.singleton 0 "some")
         ; "b", IndexedArray (IntMap.singleton 0 "hello2")
         ; "c", IndexedArray (IntMap.singleton 0 "hello3")
         ])
;;

let%test _ =
  test_ok
    {|
  function what() {
    echo hellowht
  }
  >some.txt what
  a=`cat some.txt`
|}
    ~global_vars:(StrMap.from_list [ "a", IndexedArray (IntMap.singleton 0 "hellowht") ])
;;

let%test _ =
  test_ok
    {|
  function what() {
    out1=s1.txt
    echo hellowht >$out1
    a=`cat $out1`
    echo notecho2
  }
  out2=s2.txt
  >$out2 what
  d=`cat $out2`
  f=`echo hello`
|}
    ~global_vars:
      (StrMap.from_list
         [ "out1", IndexedArray (IntMap.singleton 0 "s1.txt")
         ; "out2", IndexedArray (IntMap.singleton 0 "s2.txt")
         ; "a", IndexedArray (IntMap.singleton 0 "hellowht")
         ; "d", IndexedArray (IntMap.singleton 0 "notecho2")
         ; "f", IndexedArray (IntMap.singleton 0 "hello")
         ])
;;

(* it works! *)
(* let%test _ = test_ok {|ls >dirlist 2>&1|} *)

let%test _ =
  test_ok
    {|c=27 && echo hello | cat|}
    ~global_vars:(StrMap.from_list [ "c", IndexedArray (IntMap.singleton 0 "27") ])
;;

let%test _ =
  test_ok
    {|c=27 && d=28 && echo hello | cat|}
    ~global_vars:
      (StrMap.from_list
         [ "c", IndexedArray (IntMap.singleton 0 "27")
         ; "d", IndexedArray (IntMap.singleton 0 "28")
         ])
;;

let%test _ =
  test_ok
    {|c=27 && e=28 && echo hello | e=2 && d=10|}
    ~global_vars:
      (StrMap.from_list
         [ "c", IndexedArray (IntMap.singleton 0 "27")
         ; "e", IndexedArray (IntMap.singleton 0 "28")
         ])
;;

let%test _ =
  test_ok
    {|c=27 || e=28 && echo hello | e=2 && d=10|}
    ~global_vars:(StrMap.from_list [ "c", IndexedArray (IntMap.singleton 0 "27") ])
;;

let%test _ =
  test_ok
    {|c=27 || e=28 && echo hello | e=2 && d=10|}
    ~global_vars:(StrMap.from_list [ "c", IndexedArray (IntMap.singleton 0 "27") ])
;;

let%test _ =
  test_ok
    {|
    for e in some what;
    do
      echo $e
    done>somewhat.txt
    a=`cat somewhat.txt`|}
    ~global_vars:
      (StrMap.from_list
         [ "e", IndexedArray (IntMap.singleton 0 "what")
         ; "a", IndexedArray (IntMap.singleton 0 "some\nwhat")
         ])
;;
