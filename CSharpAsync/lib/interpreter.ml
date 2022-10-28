(** Copyright 2021-2022, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Printf
open Interpreter_class
open Tables

type arr = ArrayInt of int array | ArrayString of string array | NoArr
[@@deriving show { with_path = false }]

type variable = {
  var_type : _type;
  arr_value : arr;
  var_key : key_t;
  var_value : values;
  is_const : bool;
  assignment_count : int;
  visibility_level : int;
}
[@@deriving show { with_path = false }]

type signal = WasBreak | WasContinue | WasReturn | WasThrown | NoSignal
[@@deriving show { with_path = false }]

type callee =
  | Eval_expr of expr * context * class_t KeyMap.t
  | Eval_post_operation of context
  | Eval_stmt of stmt * context * class_t KeyMap.t
[@@deriving show { with_path = false }]

and context = {
  variable_map : variable KeyMap.t;
  current_method_type : _type;
  current_method_async : bool;
  last_expr_result : values;
  runtime_signal : signal;
  count_of_nested_cycles : int;
  visibility_level : int;
  post_inc : key_t list;
  post_dec : key_t list;
}
[@@deriving show { with_path = false }]

module Interpreter (M : MONADERROR) = struct
  open M

  let thread_data = Hashtbl.create 100
  (* to return async task results *)
  (*let thread_data = [] *)

  let init_context variable_map =
    return
      {
        variable_map;
        current_method_type = TypeVoid;
        current_method_async = true (* for main *);
        last_expr_result = ValVoid;
        runtime_signal = NoSignal;
        count_of_nested_cycles = 0;
        visibility_level = 0;
        post_inc = [];
        post_dec = [];
      }

  let find_main_class (class_map : class_t KeyMap.t) =
    let class_seq = KeyMap.to_seq class_map in
    let rec iter_classes (class_s : (key_t * class_t) Seq.t) =
      match class_s () with
      | Seq.Nil -> error "There is no Main class!"
      | Seq.Cons ((_, x), xs) -> (
          match KeyMap.find_opt "Main" x.method_map with
          | None -> iter_classes xs
          | Some _ -> return x)
    in
    iter_classes class_seq

  let add_var ctx var =
    { ctx with variable_map = KeyMap.add var.var_key var ctx.variable_map }

  let replace_var ctx var_key var =
    let variable_map = KeyMap.remove var_key ctx.variable_map in
    let variable_map = KeyMap.add var_key var variable_map in
    { ctx with variable_map }

  let remove_key_post_inc ctx var_key =
    let check_var_key x = if var_key = x then None else Some x in
    let rec change_var_key il =
      match il with
      | [] -> il
      | x :: xs -> (
          match check_var_key x with
          | None -> change_var_key xs
          | Some el -> el :: change_var_key xs)
    in
    { ctx with post_inc = change_var_key ctx.post_inc }

  let remove_key_post_dec ctx var_key =
    let check_var_key x = if var_key = x then None else Some x in
    let rec change_var_key dl =
      match dl with
      | [] -> dl
      | x :: xs -> (
          match check_var_key x with
          | None -> change_var_key xs
          | Some el -> el :: change_var_key xs)
    in
    { ctx with post_dec = change_var_key ctx.post_dec }

  let find_method_monad meth_map meth_key =
    match KeyMap.find_opt meth_key meth_map with
    | None ->
        error
          (String.concat ""
             [ "There is no given method "; meth_key; " in the Main class!" ])
    | Some method_t -> return method_t

  let rec check_expr_type curr_expr ctx class_map =
    match curr_expr with
    | Add (left, right) ->
        check_expr_type left ctx class_map >>= fun left_type ->
        let pm_left = function
          | TypeInt ->
              check_expr_type right ctx class_map >>= fun right_type ->
              let pm_right = function
                | TypeInt -> return TypeInt
                | String ->
                    return
                      String (*because we can write this: string a = 3 + "b";*)
                | _ ->
                    error
                      "Incorrect type: the right side of the expression should \
                       be Int or String!"
              in
              pm_right right_type
          | String ->
              check_expr_type right ctx class_map >>= fun right_type ->
              let pm_right = function
                | TypeInt | String -> return String
                | _ ->
                    error
                      "Incorrect type: the right side of the expression should \
                       be Int or String!"
              in
              pm_right right_type
          | _ ->
              error
                "Incorrect type: the expression Add could only be with Int or \
                 String type!"
        in
        pm_left left_type
    | Sub (left, right)
    | Div (left, right)
    | Mod (left, right)
    | Mul (left, right) ->
        check_expr_type left ctx class_map >>= fun left_type ->
        let pm_left = function
          | TypeInt ->
              check_expr_type right ctx class_map >>= fun right_type ->
              let pm_right = function
                | TypeInt -> return TypeInt
                | _ -> error "Incorrect type: the type should be Int!"
              in
              pm_right right_type
          | _ -> error "Incorrect type: the type should be Int!"
        in
        pm_left left_type
    | PostDec value | PostInc value | PreDec value | PreInc value ->
        check_expr_type value ctx class_map >>= fun value_type ->
        let pm = function
          | TypeInt -> return TypeInt
          | _ -> error "Incorrect type: the type should be Int!"
        in
        pm value_type
    | And (left, right) | Or (left, right) ->
        check_expr_type left ctx class_map >>= fun left_type ->
        let pm_left = function
          | TypeBool ->
              check_expr_type right ctx class_map >>= fun right_type ->
              let pm_right = function
                | TypeBool -> return TypeBool
                | _ -> error "Incorrect type: the type should be Bool!"
              in
              pm_right right_type
          | _ -> error "Incorrect type: the type should be Bool!"
        in
        pm_left left_type
    | Await expr -> check_expr_type expr ctx class_map
    | Not value ->
        check_expr_type value ctx class_map >>= fun value_type ->
        let pm = function
          | TypeBool -> return TypeBool
          | _ -> error "Incorrect type: the type should be Bool!"
        in
        pm value_type
    | Less (left, right)
    | More (left, right)
    | LessEqual (left, right)
    | MoreEqual (left, right) ->
        check_expr_type left ctx class_map >>= fun left_type ->
        let pm_left = function
          | TypeInt ->
              check_expr_type right ctx class_map >>= fun right_type ->
              let pm_right = function
                | TypeInt -> return TypeBool
                | _ -> error "Incorrect type: the type should be Int!"
              in
              pm_right right_type
          | _ -> error "Incorrect type: the type should be Int!"
        in
        pm_left left_type
    | Equal (left, right) | NonEqual (left, right) ->
        check_expr_type left ctx class_map >>= fun left_type ->
        let pm_left = function
          | TypeInt ->
              check_expr_type right ctx class_map >>= fun right_type ->
              let pm_right = function
                | TypeInt -> return TypeBool
                | _ ->
                    error
                      "Incorrect type: the right side of the expression should \
                       be Int!"
              in
              pm_right right_type
          | String ->
              check_expr_type right ctx class_map >>= fun right_type ->
              let pm_right = function
                | String -> return TypeBool
                | _ ->
                    error
                      "Incorrect type: the right side of the expression should \
                       be String!"
              in
              pm_right right_type
          | TypeBool ->
              check_expr_type right ctx class_map >>= fun right_type ->
              let pm_right = function
                | TypeBool -> return TypeBool
                | _ ->
                    error
                      "Incorrect type: the right side of the expression should \
                       be Bool!"
              in
              pm_right right_type
          | TypeClass _ ->
              check_expr_type right ctx class_map >>= fun right_type ->
              let pm_right = function
                | TypeClass _ -> return TypeBool
                | _ -> error "Incorrect class type!"
              in
              pm_right right_type
          | _ ->
              error
                "Incorrect type: the type should be Int, String, bool or \
                 CSharp class!"
        in
        pm_left left_type
    | Null -> return (TypeClass "null")
    | FuncCall (method_key, _) ->
        find_main_class class_map >>= fun curr_class ->
        find_method_monad curr_class.method_map method_key >>= fun mth ->
        return mth.method_type
    | Var var_key -> (
        match KeyMap.find_opt var_key ctx.variable_map with
        | None -> error ("There is no the variable " ^ var_key)
        | Some variable -> return variable.var_type)
    | ArrayAccess (Var var_key, _) -> (
        match KeyMap.find_opt var_key ctx.variable_map with
        | None -> error ("There is no the variable " ^ var_key)
        | Some variable -> return variable.var_type)
    | ArrayAccess (_, _) -> error "Wrong array access"
    | ConstExpr value -> (
        match value with
        | ValInt _ -> return TypeInt
        | ValBool _ -> return TypeBool
        | ValString _ -> return String
        | ValClass ObjNull -> return (TypeClass "null")
        | ValClass (ObjRef (class_key, _)) -> return (TypeClass class_key)
        | _ -> error "Incorrect const expression type!")
    | Assign (left, right) ->
        check_expr_type left ctx class_map >>= fun left_type ->
        let pm_left = function
          | TypeVoid -> error "Incorrect type: cannot assign to void"
          | TypeClass left_key ->
              check_expr_type right ctx class_map >>= fun right_type ->
              let pm_right = function
                | TypeClass "null" -> return (TypeClass left_key)
                | TypeClass right_key -> return (TypeClass right_key)
                | _ -> error "Incorrect type assignment!"
              in
              pm_right right_type
          | _ ->
              check_expr_type right ctx class_map >>= fun right_type ->
              if left_type = right_type then return right_type
              else error "Incorrect type assignment!"
        in
        pm_left left_type
    | _ -> error "This expression is not implemented!"

  let expr_in_stmt = function
    | PostDec _ | PostInc _ | PreDec _ | PreInc _
    | FuncCall (_, _) (* |Access (_, FuncCall (_, _)) *)
    | Assign (_, _) ->
        true
    | _ -> false

  let rec eval_stmt stmt in_ctx class_map =
    match stmt with
    | Expr expr ->
        call_func (Eval_post_operation in_ctx) >>= fun in_ctx ->
        if expr_in_stmt expr then
          call_func (Eval_expr (expr, in_ctx, class_map)) >>= fun new_ctx ->
          return new_ctx
        else error "Incorrect expression for statement"
    | StmtsBlock stat_list ->
        let rec eval_stmt_bl : stmt list -> context -> context M.t =
         fun stl ctx ->
          match stl with
          | [] -> return ctx
          | st :: tail -> (
              match st with
              | (Break | Continue | Return _) when tail <> [] ->
                  error "Unreachable code"
              | _
                when ctx.count_of_nested_cycles >= 1
                     && ctx.runtime_signal = WasBreak ->
                  return ctx
              | _
                when ctx.count_of_nested_cycles >= 1
                     && ctx.runtime_signal = WasContinue ->
                  return ctx
              | _ when ctx.runtime_signal = WasReturn -> return ctx
              | _ when ctx.runtime_signal = WasThrown -> return ctx
              | _ ->
                  call_func (Eval_stmt (st, ctx, class_map)) >>= fun head_ctx ->
                  eval_stmt_bl tail head_ctx)
        in
        eval_stmt_bl stat_list in_ctx >>= fun new_ctx ->
        call_func (Eval_post_operation new_ctx) >>= fun new_ctx ->
        return new_ctx
    | If (expr, then_stat, else_stat_opt) -> (
        call_func (Eval_expr (expr, in_ctx, class_map)) >>= fun if_ctx ->
        call_func (Eval_post_operation if_ctx) >>= fun if_ctx ->
        match if_ctx.last_expr_result with
        | ValBool true -> (
            match then_stat with
            | StmtsBlock _ ->
                call_func
                  (Eval_stmt
                     ( then_stat,
                       {
                         if_ctx with
                         visibility_level = if_ctx.visibility_level + 1;
                       },
                       class_map ))
                >>= fun in_ctx ->
                return
                  { in_ctx with visibility_level = in_ctx.visibility_level - 1 }
            | _ -> call_func (Eval_stmt (then_stat, if_ctx, class_map)))
        | ValBool false -> (
            match else_stat_opt with
            | Some (StmtsBlock _ as else_stat) ->
                call_func
                  (Eval_stmt
                     ( else_stat,
                       {
                         if_ctx with
                         visibility_level = if_ctx.visibility_level + 1;
                       },
                       class_map ))
                >>= fun else_ctx ->
                return
                  {
                    else_ctx with
                    visibility_level = else_ctx.visibility_level - 1;
                  }
            | Some else_stat ->
                call_func (Eval_stmt (else_stat, if_ctx, class_map))
            | None -> return in_ctx)
        | _ -> error "Incorrect type for condition statement")
    | While (expr, stmt) -> (
        let rec eval_loop loop_stat ctx =
          (* Check the break, if it happened - we would exit the cycle *)
          if ctx.runtime_signal = WasBreak then
            match loop_stat with
            (* If there was a StatementBlock, then we still need to lower the visibility level *)
            | StmtsBlock _ ->
                return
                  {
                    ctx with
                    runtime_signal = NoSignal;
                    count_of_nested_cycles = ctx.count_of_nested_cycles - 1;
                    visibility_level = ctx.visibility_level - 1;
                  }
            | _ ->
                return
                  {
                    ctx with
                    runtime_signal = NoSignal;
                    count_of_nested_cycles = ctx.count_of_nested_cycles - 1;
                  }
          else
            call_func (Eval_expr (expr, ctx, class_map)) >>= fun new_ctx ->
            call_func (Eval_post_operation new_ctx) >>= fun new_ctx ->
            match new_ctx.last_expr_result with
            | ValBool false -> (
                match loop_stat with
                | StmtsBlock _ ->
                    return
                      {
                        new_ctx with
                        count_of_nested_cycles = ctx.count_of_nested_cycles - 1;
                        visibility_level = ctx.visibility_level - 1;
                      }
                | _ ->
                    return
                      {
                        new_ctx with
                        count_of_nested_cycles = ctx.count_of_nested_cycles - 1;
                      })
            | ValBool true -> (
                call_func (Eval_stmt (loop_stat, new_ctx, class_map))
                >>= fun loop_ctx ->
                match loop_ctx.runtime_signal with
                (* If we have return  - we interrupt everything and return the context *)
                | WasReturn -> return loop_ctx
                (* Else, we have continue and can cycle again*)
                | WasContinue ->
                    eval_loop loop_stat
                      { loop_ctx with runtime_signal = NoSignal }
                | _ -> eval_loop loop_stat loop_ctx)
            | _ -> error "Incorrect expression type for while stametent"
        in
        match stmt with
        | StmtsBlock _ ->
            eval_loop stmt
              {
                in_ctx with
                count_of_nested_cycles = in_ctx.count_of_nested_cycles + 1;
                visibility_level = in_ctx.visibility_level + 1;
              }
        | _ ->
            eval_loop stmt
              {
                in_ctx with
                count_of_nested_cycles = in_ctx.count_of_nested_cycles + 1;
              })
    | For (stat_opt, expr_opt, after_list, body_stat) ->
        (* With a loop for visibility_level always increases, despite the presence/absence of a body block *)
        (match stat_opt with
        | None ->
            return
              { in_ctx with visibility_level = in_ctx.visibility_level + 1 }
        | Some dec_stat ->
            eval_stmt dec_stat
              { in_ctx with visibility_level = in_ctx.visibility_level + 1 }
              class_map)
        >>= fun new_ctx ->
        call_func (Eval_post_operation new_ctx) >>= fun new_ctx ->
        let remove_loop_vars (ctx : context) =
          let var_seq = KeyMap.to_seq ctx.variable_map in
          let rec iter_classes (var_s : (key_t * variable) Seq.t)
              (ctx : context) =
            match var_s () with
            | Seq.Nil -> return ctx
            | Seq.Cons ((key, x), xs) -> (
                match x.visibility_level = ctx.visibility_level with
                | true ->
                    iter_classes xs
                      {
                        ctx with
                        variable_map = KeyMap.remove key ctx.variable_map;
                      }
                | false -> iter_classes xs ctx)
          in
          iter_classes var_seq ctx >>= fun ctx -> return ctx
        in
        let rec eval_loop body_st af_list ctx =
          (* Standard: we look at the result of the boolean expression, if true - calculate
             the body and increments after *)
          (match expr_opt with
          | None -> return { ctx with last_expr_result = ValBool true }
          | Some expr_t -> call_func (Eval_expr (expr_t, ctx, class_map)))
          >>= fun cond_ctx ->
          call_func (Eval_post_operation cond_ctx) >>= fun cond_ctx ->
          match cond_ctx.last_expr_result with
          (* If fail, it means we are no longer cycling, we return the context with a reduced
             counter of nested_cycle and visibility_level*)
          | ValBool false ->
              remove_loop_vars
                {
                  cond_ctx with
                  count_of_nested_cycles = cond_ctx.count_of_nested_cycles - 1;
                  visibility_level = cond_ctx.visibility_level - 1;
                }
          | ValBool true -> (
              let rec interpret_expr_list e_list as_ctx =
                match e_list with
                | [] ->
                    call_func (Eval_post_operation as_ctx) >>= fun as_ctx ->
                    return as_ctx
                | x :: xs ->
                    if expr_in_stmt x then
                      call_func (Eval_expr (x, as_ctx, class_map))
                      >>= fun next_ctx -> interpret_expr_list xs next_ctx
                    else error "Incorrect expression for after body list"
              in
              (* Variables inside the block itself will be in a larger visibility_level than
                 from the initializer *)
              call_func
                (Eval_stmt
                   ( body_st,
                     {
                       cond_ctx with
                       visibility_level = new_ctx.visibility_level + 1;
                     },
                     class_map ))
              >>= fun body_ctx ->
              match body_ctx.runtime_signal with
              (* If we have return  - we interrupt everything and we return the context *)
              | WasReturn -> return body_ctx
              (* We can have continue - so we cycle again*)
              | WasContinue ->
                  interpret_expr_list af_list body_ctx >>= fun after_ctx ->
                  eval_loop body_st af_list
                    { after_ctx with runtime_signal = NoSignal }
              (* Check the break, whether it happened, happened - we exit the cycle *)
              | WasBreak ->
                  remove_loop_vars
                    {
                      ctx with
                      runtime_signal = NoSignal;
                      count_of_nested_cycles = ctx.count_of_nested_cycles - 1;
                      visibility_level = ctx.visibility_level - 1;
                    }
              | _ ->
                  interpret_expr_list af_list body_ctx >>= fun after_ctx ->
                  eval_loop body_st af_list after_ctx)
          | _ -> error "Incorrect condition type in for statement"
        in
        eval_loop body_stat after_list
          {
            new_ctx with
            count_of_nested_cycles = in_ctx.count_of_nested_cycles + 1;
          }
    | Break ->
        if in_ctx.count_of_nested_cycles <= 0 then
          error "There is no loop to do break"
        else return { in_ctx with runtime_signal = WasBreak }
    | Continue ->
        if in_ctx.count_of_nested_cycles <= 0 then
          error "There is no loop to do continue"
        else return { in_ctx with runtime_signal = WasContinue }
    | Return None when in_ctx.current_method_type = TypeVoid ->
        call_func (Eval_post_operation in_ctx) >>= fun in_ctx ->
        (* If the type is Void, we exit with the Void value set by the signal that was return *)
        return
          { in_ctx with last_expr_result = ValVoid; runtime_signal = WasReturn }
    | Return None -> error "Return value type mismatch"
    | Return (Some expr) ->
        check_expr_type expr in_ctx class_map >>= fun ret_type ->
        if ret_type <> in_ctx.current_method_type then
          error "Return value type mismatch"
        else
          (* We return the context in which there is the result of the expression
             and set the signal that was return *)
          call_func (Eval_expr (expr, in_ctx, class_map)) >>= fun new_ctx ->
          call_func (Eval_post_operation new_ctx) >>= fun new_ctx ->
          return { new_ctx with runtime_signal = WasReturn }
    | VarDeclr (modifier, vars_type, var_list) ->
        let is_const : modifier option -> bool = function
          | Some Const -> true
          | _ -> false
        in
        let get_base_value = function
          | TypeInt -> ValInt 0
          | String -> ValString ""
          | TypeClass _ -> ValNull
          | TypeBool -> ValBool false
          | TypeVoid -> ValVoid
          | _ -> ValInt 0
        in
        let rec interpret_var var_list var_ctx =
          match var_list with
          | [] -> return var_ctx
          | (var_name, var_expr_opt) :: tail ->
              (match var_expr_opt with
              (* If there is nothing, initialize with the base value *)
              | None ->
                  let var =
                    match vars_type with
                    | Array (tip, dim) ->
                        let newArr =
                          match tip with
                          | TypeInt -> ArrayInt (Array.make dim 0)
                          | _ -> ArrayString (Array.make dim "")
                        in
                        {
                          var_key = var_name;
                          var_type = tip;
                          arr_value = newArr;
                          var_value = get_base_value vars_type;
                          is_const = is_const modifier;
                          assignment_count = 0;
                          visibility_level = var_ctx.visibility_level;
                        }
                    | _ ->
                        {
                          var_key = var_name;
                          var_type = vars_type;
                          arr_value = NoArr;
                          var_value = get_base_value vars_type;
                          is_const = is_const modifier;
                          assignment_count = 0;
                          visibility_level = var_ctx.visibility_level;
                        }
                  in
                  return (add_var var_ctx var)
              (* If there is something, we assign the value calculated on the right *)
              | Some var_expr -> (
                  check_expr_type var_expr var_ctx class_map
                  >>= fun var_expr_type ->
                  (* Add to the context variables map what is in the variable expression on the right *)
                  match var_expr_type with
                  | _ when var_expr_type = vars_type ->
                      call_func (Eval_expr (var_expr, var_ctx, class_map))
                      >>= fun expr_ctx ->
                      call_func (Eval_post_operation expr_ctx)
                      >>= fun expr_ctx ->
                      let var =
                        {
                          var_key = var_name;
                          var_type = var_expr_type;
                          arr_value = NoArr;
                          var_value = expr_ctx.last_expr_result;
                          is_const = is_const modifier;
                          assignment_count = 1;
                          visibility_level = expr_ctx.visibility_level;
                        }
                      in
                      return (add_var expr_ctx var)
                  | _ ->
                      error
                        ("Incorrect value type for declared variable:"
                       ^ show__type var_expr_type)))
              (* TODO: Fix this bug *)
              >>= fun next_ctx -> interpret_var tail next_ctx
        in
        interpret_var var_list in_ctx
    | Print print_expr ->
        call_func (Eval_expr (print_expr, in_ctx, class_map)) >>= fun new_ctx ->
        call_func (Eval_post_operation new_ctx) >>= fun new_ctx ->
        let eval_printer = function
          | ValInt value -> return (printf "%d\n" value)
          | ValBool value -> return (printf "%b\n" value)
          | ValString value -> return (printf "%s\n" value)
          | ValClass value -> (
              match value with
              | ObjNull -> error "NullReferenceException"
              | ObjRef (class_key, _) -> return (printf "%s\n" class_key))
          | ValVoid -> error "void"
          | ValNull -> error "null"
        in
        eval_printer new_ctx.last_expr_result >> return new_ctx

  and call_func x =
    (* reserved to construct a stack of calls *)
    match x with
    | Eval_post_operation cont -> eval_post_operation cont
    | Eval_expr (e, cont, k) -> eval_expr e cont k
    | Eval_stmt (st, cont, k) -> eval_stmt st cont k

  and run_in_parallel x =
    let self = Thread.id (Thread.self ()) in
    (*Printf.printf "[we run task in thr %d:]\n" self; *)
    let rez =
      match x with
      (* execute task *)
      | Eval_post_operation cont -> eval_post_operation cont
      | Eval_expr (e, cont, k) -> eval_expr e cont k
      | Eval_stmt (st, cont, k) -> eval_stmt st cont k
    in
    (*List.append thread_data [ (self, rez) ] *)
    Hashtbl.add thread_data self rez (* store return value *)

  and eval_expr in_expr in_ctx class_map =
    let eval_helper e_expr ctx =
      let eval_left_right left_e right_e eval_f =
        eval_post_operation ctx >>= fun ctx ->
        eval_expr left_e ctx class_map >>= fun left_ctx ->
        eval_expr right_e left_ctx class_map >>= fun right_ctx ->
        try
          let ret_v =
            eval_f left_ctx.last_expr_result right_ctx.last_expr_result
          in
          return { right_ctx with last_expr_result = ret_v }
        with Invalid_argument m -> error m
      in
      match e_expr with
      | Add (left, right) ->
          eval_left_right left right (fun leftv rightv ->
              match (leftv, rightv) with
              | ValInt x, ValInt y -> ValInt (x + y)
              | ValString x, ValString y -> ValString (x ^ y)
              | ValInt x, ValString y -> ValString (string_of_int x ^ y)
              | ValString x, ValInt y -> ValString (x ^ string_of_int y)
              | _, _ ->
                  raise
                    (Invalid_argument "Incorrect argument types for adding!"))
      | Sub (left, right) ->
          eval_left_right left right (fun leftv rightv ->
              match (leftv, rightv) with
              | ValInt x, ValInt y -> ValInt (x - y)
              | _, _ ->
                  raise
                    (Invalid_argument
                       "Incorrect argument types for subtraction!"))
      | Mul (left, right) ->
          eval_left_right left right (fun leftv rightv ->
              match (leftv, rightv) with
              | ValInt x, ValInt y -> ValInt (x * y)
              | _, _ ->
                  raise
                    (Invalid_argument
                       "Incorrect argument types for multiplication!"))
      | Div (left, right) ->
          eval_left_right left right (fun leftv rightv ->
              match (leftv, rightv) with
              | ValInt _, ValInt y when y = 0 ->
                  raise (Invalid_argument "Division by zero!")
              | ValInt x, ValInt y -> ValInt (x / y)
              | _, _ ->
                  raise
                    (Invalid_argument "Incorrect argument types for division!"))
      | Mod (left, right) ->
          eval_left_right left right (fun leftv rightv ->
              match (leftv, rightv) with
              | ValInt _, ValInt y when y = 0 ->
                  raise (Invalid_argument "Division by zero!")
              | ValInt x, ValInt y -> ValInt (x mod y)
              | _, _ ->
                  raise
                    (Invalid_argument
                       "Incorrect argument types for mod operator!"))
      | And (left, right) ->
          eval_left_right left right (fun leftv rightv ->
              match (leftv, rightv) with
              | ValBool x, ValBool y -> ValBool (x && y)
              | _, _ ->
                  raise
                    (Invalid_argument
                       "Incorrect types for logical and operator!"))
      | Or (left, right) ->
          eval_left_right left right (fun leftv rightv ->
              match (leftv, rightv) with
              | ValBool x, ValBool y -> ValBool (x || y)
              | _, _ ->
                  raise
                    (Invalid_argument "Incorrect types for logical or operator!"))
      | Await expr -> (
          match ctx.current_method_async with
          | true ->
              let th =
                Thread.create run_in_parallel
                  (Eval_expr (expr, in_ctx, class_map))
              in
              (* say to run a function in parallel *)
              Thread.join th;
              (* wait *)
              (*let x = List.find (fun t -> fst t = Thread.id th) thread_data in
                snd x *)
              Hashtbl.find thread_data (Thread.id th)
              (* obtain and return a result *)
          | _ -> error "Attempt to run from a non-async context")
      | Not not_expr -> (
          eval_post_operation ctx >>= fun ctx ->
          eval_expr not_expr ctx class_map >>= fun new_ctx ->
          match new_ctx.last_expr_result with
          | ValBool x ->
              return { new_ctx with last_expr_result = ValBool (not x) }
          | _ -> error "Incorrect types for logical not operator!")
      | Less (left, right) ->
          eval_left_right left right (fun leftv rightv ->
              match (leftv, rightv) with
              | ValInt x, ValInt y -> ValBool (x < y)
              | _ ->
                  raise
                    (Invalid_argument "Incorrect type for comparison operator!"))
      | More (left, right) -> eval_expr (Less (right, left)) ctx class_map
      | LessEqual (left, right) ->
          eval_expr (Not (More (left, right))) ctx class_map
      | MoreEqual (left, right) ->
          eval_expr (Not (Less (left, right))) ctx class_map
      | Equal (left, right) ->
          eval_left_right left right (fun leftv rightv ->
              match (leftv, rightv) with
              | ValInt x, ValInt y -> ValBool (x = y)
              | ValBool x, ValBool y -> ValBool (x = y)
              | ValVoid, ValVoid -> ValBool true
              | ValString x, ValString y -> ValBool (x = y)
              | ValClass x, ValClass y -> (
                  match (x, y) with
                  | ObjNull, ObjNull -> ValBool true
                  | ObjNull, _ | _, ObjNull -> ValBool false
                  | ObjRef (key1, _), ObjRef (key2, _) -> ValBool (key1 = key2))
              | _ -> raise (Invalid_argument "Incorrect types for equality!"))
      | NonEqual (left, right) ->
          eval_expr (Not (Equal (left, right))) ctx class_map
      | ConstExpr value ->
          eval_post_operation ctx >>= fun ctx ->
          return { ctx with last_expr_result = value }
      | ArrayAccess (Var var_key, idx) -> (
          eval_expr idx ctx class_map >>= fun idx_ctx ->
          match idx_ctx.last_expr_result with
          | ValInt idx_val -> (
              eval_post_operation ctx >>= fun ctx ->
              match KeyMap.find_opt var_key ctx.variable_map with
              | Some var -> (
                  let vval =
                    match var.arr_value with
                    | ArrayInt v ->
                        if idx_val >= Array.length v then None
                        else Some (ValInt v.(idx_val))
                    | ArrayString v ->
                        if idx_val >= Array.length v then None
                        else Some (ValString v.(idx_val))
                    | _ -> None
                  in
                  match vval with
                  | Some retval -> return { ctx with last_expr_result = retval }
                  | None -> error "Index out of bounds")
              | None ->
                  error
                    (String.concat ""
                       [ "The varibale "; var_key; " is not found!" ]))
          | _ -> error "test non int expr result type")
      | Var var_key -> (
          eval_post_operation ctx >>= fun ctx ->
          match KeyMap.find_opt var_key ctx.variable_map with
          | Some var -> return { ctx with last_expr_result = var.var_value }
          | None ->
              error
                (String.concat ""
                   [ "The varibale "; var_key; " is not found!" ]))
      | Null -> return { ctx with last_expr_result = ValClass ObjNull }
      | FuncCall (method_key, args) -> (
          find_main_class class_map >>= fun main_class ->
          find_method_monad main_class.method_map method_key >>= fun meth ->
          fill_var_map KeyMap.empty ctx args meth.args class_map
          >>= fun (new_var_map, new_ctx) ->
          eval_post_operation new_ctx >>= fun new_ctx ->
          let if_async = List.mem Async meth.method_mod in
          (*if it has the async modifier *)
          eval_stmt meth.body
            {
              variable_map = new_var_map;
              current_method_type = meth.method_type;
              current_method_async = if_async;
              last_expr_result = ValVoid;
              runtime_signal = NoSignal;
              count_of_nested_cycles = 0;
              visibility_level = 0;
              post_inc = [];
              post_dec = [];
            }
            class_map
          >>= fun res_ctx ->
          match res_ctx.runtime_signal with
          | WasThrown ->
              return
                {
                  new_ctx with
                  last_expr_result = res_ctx.last_expr_result;
                  runtime_signal = WasThrown;
                }
          | _ ->
              return
                {
                  new_ctx with
                  last_expr_result =
                    (if meth.method_type = TypeVoid then ValVoid
                    else res_ctx.last_expr_result);
                })
      (* eval index; then save the val_expr result to the index position in array-variable var_key *)
      | Assign (ArrayAccess (Var var_key, index_expr), val_expr) -> (
          eval_post_operation ctx >>= fun ctx ->
          eval_expr val_expr ctx class_map >>= fun assign_ctx ->
          match KeyMap.find_opt var_key assign_ctx.variable_map with
          | None -> error "Variable not found"
          | Some old_var -> (
              eval_expr index_expr assign_ctx class_map >>= fun idx_ctx ->
              match idx_ctx.last_expr_result with
              | ValInt idx_val -> (
                  check_const_assign_variable old_var >>= fun _ ->
                  let cur_arr = old_var.arr_value in
                  let new_arr =
                    match (cur_arr, assign_ctx.last_expr_result) with
                    | ArrayInt ar_inst, ValInt v ->
                        if idx_val < Array.length ar_inst then (
                          ar_inst.(idx_val) <- v;
                          ArrayInt ar_inst)
                        else NoArr
                    | ArrayString ar_inst, ValString v ->
                        if idx_val < Array.length ar_inst then (
                          ar_inst.(idx_val) <- v;
                          ArrayString ar_inst)
                        else NoArr
                    | _ -> NoArr
                  in
                  match new_arr with
                  | NoArr -> error "Wrong array type"
                  | _ ->
                      let var =
                        {
                          old_var with
                          arr_value = new_arr;
                          assignment_count = old_var.assignment_count + 1;
                        }
                      in
                      return (replace_var assign_ctx var_key var))
              | _ -> error "Non int index"))
      | Assign (Var var_key, val_expr) -> (
          eval_post_operation ctx >>= fun ctx ->
          eval_expr val_expr ctx class_map >>= fun assign_ctx ->
          match KeyMap.find_opt var_key assign_ctx.variable_map with
          | None -> error "Variable not found"
          | Some old_var ->
              check_const_assign_variable old_var >>= fun _ ->
              let var =
                {
                  old_var with
                  var_value = assign_ctx.last_expr_result;
                  assignment_count = old_var.assignment_count + 1;
                }
              in
              return (replace_var assign_ctx var_key var))
      | PreInc (Var var_key) ->
          eval_expr
            (Assign (Var var_key, Add (Var var_key, ConstExpr (ValInt 1))))
            ctx class_map
      | PreDec (Var var_key) ->
          eval_expr
            (Assign (Var var_key, Sub (Var var_key, ConstExpr (ValInt 1))))
            ctx class_map
      | PostInc (Var var_key) ->
          eval_expr
            (Assign (Var var_key, Add (Var var_key, ConstExpr (ValInt 0))))
            ctx class_map
          >>= fun ctx -> return { ctx with post_inc = var_key :: ctx.post_inc }
      | PostDec (Var var_key) ->
          eval_expr
            (Assign (Var var_key, Sub (Var var_key, ConstExpr (ValInt 0))))
            ctx class_map
          >>= fun ctx -> return { ctx with post_dec = var_key :: ctx.post_dec }
      | _ -> error "Incorrect expression!"
    in
    eval_helper in_expr in_ctx

  and eval_post_operation ctx =
    let rec eval_post_inc_dec post_ctx inc_v remove_f = function
      | [] -> return post_ctx
      | x :: xs -> (
          match KeyMap.find_opt x post_ctx.variable_map with
          | None -> error (String.concat "" [ "Variable "; x; " not found!" ])
          | Some old_var ->
              check_const_assign_variable old_var >>= fun _ ->
              let change_value =
                match old_var.var_value with
                | ValInt v -> return (ValInt (v + inc_v))
                | _ -> error "Variable in post inc shoud be integer!"
              in
              change_value >>= fun value ->
              let var =
                {
                  old_var with
                  var_value = value;
                  assignment_count = old_var.assignment_count + 1;
                }
              in
              return (replace_var post_ctx x var) >>= fun new_ctx ->
              eval_post_inc_dec (remove_f new_ctx x) inc_v remove_f xs)
    in
    eval_post_inc_dec ctx 1 remove_key_post_inc ctx.post_inc >>= fun inc_ctx ->
    eval_post_inc_dec inc_ctx (-1) remove_key_post_dec inc_ctx.post_dec
    >>= fun dec_ctx -> return dec_ctx

  and fill_var_map varm ctx args meth_args class_map =
    let update_var var_ctx arg = function
      | var_type, var_key ->
          eval_expr arg var_ctx class_map >>= fun new_ctx ->
          let var =
            {
              var_type;
              arr_value = NoArr;
              var_key;
              is_const = false;
              assignment_count = 1;
              var_value = new_ctx.last_expr_result;
              visibility_level = 0;
            }
          in
          let add_ctx = add_var new_ctx var in
          return (add_ctx.variable_map, add_ctx)
    in
    let rec iter_vars (var_map, var_ctx) var_args var_meth_args =
      match (var_args, var_meth_args) with
      | [], [] -> return (var_map, var_ctx)
      | x :: xs, y :: ys ->
          update_var var_ctx x y >>= fun (new_vm, new_ctx) ->
          iter_vars (new_vm, new_ctx) xs ys
      | _, _ -> error "Incorect var list in the method!"
    in
    iter_vars (varm, ctx) args meth_args

  and check_const_assign_variable : variable -> unit M.t =
   fun var ->
    match var.assignment_count with
    | 0 -> return ()
    | _ when not var.is_const -> return ()
    | _ -> error "Assigment to a constant variable"

  let start_interpreting : class_t KeyMap.t -> context M.t =
   fun class_map ->
    find_main_class class_map >>= fun main_class ->
    init_context KeyMap.empty >>= fun ctx ->
    let main = KeyMap.find "Main" main_class.method_map in
    eval_stmt main.body ctx class_map >>= fun final_ctx ->
    match final_ctx.runtime_signal = WasThrown with
    | false -> return final_ctx
    | true -> error "Unhandled exception"
end
