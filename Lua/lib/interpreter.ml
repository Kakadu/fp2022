(** Copyright 2021-2022, Arthur Alekseev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

module Interpreter : sig
  type context

  type result =
    | Done of context
    | Fail of string

  val interpret : Ast.ast -> context -> result
  val emptyctx : context
end = struct
  include Ast
  include Parser

  module rec Const : sig
    type t =
      | String of string
      | Number of float
      | Function of Ast.l_function
      | Nil
      | Table of TableMap.t
      | Bool of bool
    [@@deriving show { with_path = false }]

    val from_const : const -> t
  end = struct
    type t =
      | String of string
      | Number of float
      | Function of Ast.l_function
      | Nil
      | Table of TableMap.t
      | Bool of bool
    [@@deriving show { with_path = false }]

    let from_const = function
      | Ast.String s -> String s
      | Ast.Number n -> Number n
      | Ast.Bool b -> Bool b
      | Ast.Nil -> Nil
      | Ast.Function f -> Function f
    ;;
  end

  and TableMap : sig
    type t [@@deriving show]

    val empty : t
    val add : Const.t -> Const.t -> t -> t
    val remove : Const.t -> t -> t
    val replace : Const.t -> Const.t -> t -> t
    val find_opt : Const.t -> t -> Const.t option
    val compare : t -> t -> int
  end = struct
    include Ast

    module M = Map.Make (struct
      type t = Const.t

      let compare = compare
    end)

    type t = Const.t M.t

    let compare a b = M.compare compare a b

    let pp ppf t =
      Format.fprintf ppf "{";
      M.iter (fun k v -> Format.fprintf ppf "[%a] = %a" Const.pp k Const.pp v) t;
      Format.fprintf ppf "}"
    ;;

    let show m = Format.asprintf "%a" pp m
    let empty = M.empty
    let add = M.add
    let remove = M.remove
    let replace k v tbl = M.update k (fun _ -> Some v) tbl
    let find_opt = M.find_opt
  end

  module VarsMap : sig
    type t [@@deriving show]

    val find_opt : string -> t -> Const.t option
    val replace : string -> Const.t -> t -> t
    val empty : t
  end = struct
    module M = Map.Make (String)

    type t = Const.t M.t

    let pp ppf t = M.iter (fun k v -> Format.fprintf ppf "[%s] = %a" k Const.pp v) t
    let show m = Format.asprintf "%a" pp m
    let find_opt = M.find_opt
    let replace k v t = M.update k (fun _ -> Some v) t
    let empty = M.empty
  end

  type variables = VarsMap.t [@@deriving show { with_path = false }]

  type context =
    { vars : variables
    ; previous : context option
    ; last_exec : Const.t
    ; brk : int option
    ; ret : int option
    ; level : int
    }
  [@@deriving show { with_path = false }]

  let show_context x = Format.asprintf "%a" pp_context x

  type interpreter_result =
    | Interpreted of context (** Normal interpretation result*)
    | Error of string (** Failed interpretation result*)
    | Returning of context
        (** Context that contains context to be returned, it is skipped by an bind operator. It should be handled in a function interpreter*)
    | Breaking of context
        (** Context that contains context to be broken to, it is skipped by an bind operator. It should be handled in a loop interpreter*)

  type result =
    | Done of context
    | Fail of string

  type interpreter = context -> interpreter_result

  let return c _ = Interpreted c
  let error m _ = Error m

  let ( >>= ) : interpreter -> (context -> interpreter) -> interpreter =
   fun i f c ->
    match i c with
    | Interpreted c2 -> f c2 c2
    | e -> e
 ;;

  let return_le res ctx = return { ctx with last_exec = res }

  let rec get_var varname = function
    | None -> return_le Nil
    | Some ctx ->
      (match VarsMap.find_opt varname ctx.vars with
       | Some v -> return_le v
       | None -> get_var varname ctx.previous)
  ;;

  let rec exec_expr ex ctx =
    match ex with
    | Const c -> return_le (Const.from_const c) ctx ctx
    | Variable v -> (get_var v (Some ctx)) ctx ctx
    | TableGet (t, i) -> get_from_table ctx t i
    | TableInit el -> create_table ctx el ctx
    | UnOp (op, le) -> exec_un_op op le ctx
    | BinOp (op, le, re) -> exec_bin_op op le re ctx
    | ExprApply apply -> exec_fun apply ctx ctx

  and exec_fun apply ctx =
    (* Hardcoded functions *)
    let rec print_vars exprs ctx =
      match exprs with
      | [] -> return ctx
      | h :: t ->
        exec_expr h
        >>= fun h_res ->
        (match h_res.last_exec with
         | String s -> print_string s
         | Number n -> print_float n
         | Function _ -> print_string "<function>"
         | Nil -> print_string "nil"
         | Bool v -> print_string (if v then "true" else "false")
         | tbl -> print_string (Const.show tbl));
        print_endline "";
        print_vars t h_res
    in
    match apply with
    | Call (fn, args) ->
      (* Hardcoded functions *)
      (match fn with
       | Variable "print" -> print_vars args ctx
       | Variable "pctx" ->
         print_endline (show_context ctx);
         return ctx
       | _ ->
         exec_expr fn
         >>= fun expr_e ->
         return
           { ctx with
             ret = Some ctx.level
           ; vars = VarsMap.empty
           ; previous = Some ctx
           ; last_exec = Nil
           ; level = ctx.level + 1
           }
         >>= fun _ ->
         (match expr_e.last_exec with
          | Function (idents, body) ->
            exec_local_set idents args
            >>= fun c ->
            (match exec_many body c with
             | Returning ctx -> return ctx
             | Interpreted ctx ->
               return ctx
               >>= fun ctx ->
               (match ctx.previous with
                | Some p -> return { p with last_exec = Nil }
                | None -> error "Exiting from global context")
             | Error m -> error m
             | Breaking _ -> error "breaking from non loop")
          | _ -> error "You can only call functions"))

  and exec_un_op op le =
    let exec_bool_uop op last_ctx = function
      | Const.Bool x -> return { last_ctx with last_exec = Bool (op x) }
      | _ -> error "Attempt to do logic with non boolens"
    in
    let exec_num_uop op last_ctx = function
      | Const.Number x -> return { last_ctx with last_exec = Number (op x) }
      | _ -> error "Attempt to do math with non numbers"
    in
    exec_expr le
    >>= fun ler ->
    match op with
    | Not -> exec_bool_uop not ler ler.last_exec
    | USub -> exec_num_uop (fun x -> 0. -. x) ler ler.last_exec

  and exec_bin_op op le re ctx =
    (let exec_num_op op ler rer last_ctx =
       match ler, rer with
       | Const.Number x, Const.Number y ->
         return { last_ctx with last_exec = Number (op x y) }
       | _ -> error "Attempt to do math with non numbers"
     in
     let exec_str_op op ler rer last_ctx =
       match ler, rer with
       | Const.String x, Const.String y ->
         return { last_ctx with last_exec = String (op x y) }
       | _ -> error "Attempt to do concatenation with non strings"
     in
     let exec_bool_op op ler rer last_ctx =
       match ler, rer with
       | x, y -> return { last_ctx with last_exec = Bool (op (get_bool x) (get_bool y)) }
     in
     let exec_comp_op op ler rer last_ctx =
       match ler, rer with
       | x, y -> return { last_ctx with last_exec = Bool (op x y) }
     in
     exec_expr le
     >>= fun le_e ->
     exec_expr re
     >>= fun re_e ->
     let ler = le_e.last_exec in
     let rer = re_e.last_exec in
     match op with
     | AOp aop ->
       let actual_op =
         match aop with
         | Sub -> ( -. )
         | Div -> ( /. )
         | Mul -> ( *. )
         | Add -> ( +. )
         | Pow -> ( ** )
       in
       exec_num_op actual_op ler rer re_e
     | COp cop ->
       let actual_op =
         match cop with
         | Ge -> ( >= )
         | Le -> ( <= )
         | Gt -> ( > )
         | Lt -> ( < )
         | Eq -> ( = )
         | Ne -> ( <> )
       in
       exec_comp_op actual_op ler rer re_e
     | SOp Concat -> exec_str_op ( ^ ) ler rer re_e
     | LOp lop ->
       let actual_op =
         match lop with
         | And -> ( && )
         | Or -> ( || )
       in
       exec_bool_op actual_op ler rer re_e)
      ctx

  and get_from_table ctx t i =
    (exec_expr t
    >>= fun t_e ->
    exec_expr i
    >>= fun i_e ->
    match t_e.last_exec with
    | Table ht ->
      (match TableMap.find_opt i_e.last_exec ht with
       | Some v -> return { ctx with last_exec = v }
       | None -> return { ctx with last_exec = Nil })
    | _ -> error "Taking index from non table")
      ctx

  and create_table ctx kvp_e_list =
    let ht = TableMap.empty in
    let rec add_kvp tbl ictx li = function
      | [] -> return { ictx with last_exec = Table tbl }
      | h :: t ->
        (match h with
         | PairExpr (k, v) ->
           exec_expr k
           >>= fun k_e ->
           exec_expr v
           >>= fun v_e ->
           add_kvp (TableMap.replace k_e.last_exec v_e.last_exec tbl) v_e li t
         | JustExpr v ->
           exec_expr v
           >>= fun v_e ->
           add_kvp (TableMap.replace (Number li) v_e.last_exec tbl) v_e (li +. 1.) t)
    in
    add_kvp ht ctx 1. kvp_e_list

  and exec_up_ctx interpreter ctx =
    (interpreter
    >>= fun chctx ->
    match chctx.previous with
    | Some c -> return c
    | None -> error "exit from global context")
      { ctx with
        vars = VarsMap.empty
      ; previous = Some ctx
      ; last_exec = Nil
      ; level = ctx.level + 1
      }

  and exec_loop_ctx interpreter ctx =
    (fun nctx ->
      match interpreter nctx with
      | Breaking ct -> Interpreted ct
      | Returning e -> Returning e
      | Error m -> Error m
      | Interpreted ct ->
        (match ct.previous with
         | Some c -> Interpreted c
         | None -> Error "exit from global context"))
      { ctx with
        vars = VarsMap.empty
      ; previous = Some ctx
      ; brk = Some ctx.level
      ; last_exec = Nil
      ; level = ctx.level + 1
      }

  and exec_local_set idents exprs ctx =
    let set_ident id ex ctx =
      (exec_expr ex
      >>= fun ex_ctx ->
      return { ex_ctx with vars = VarsMap.replace id ex_ctx.last_exec ex_ctx.vars })
        ctx
    in
    let rec helper ctx = function
      | ih :: it, ah :: at -> set_ident ih ah >>= fun nctx -> helper nctx (it, at)
      | ih :: it, _ ->
        return ctx
        >>= fun _ -> set_ident ih (Const Nil) >>= fun nctx -> helper nctx (it, [])
      | _, _ -> return ctx
    in
    helper ctx (idents, exprs) ctx

  and exec_stat stat ctx =
    match stat with
    | Expr expr -> exec_stat (StatementApply (Call (Variable "print", [ expr ]))) ctx
    | Set (le, re) -> exec_set le re ctx
    | Local (ids, exps) -> exec_local_set ids exps ctx
    | Do block -> exec_up_ctx (exec_many block) ctx
    | If (ifexpr, block, elseif_blocks, else_block) ->
      exec_up_ctx (exec_if ifexpr block elseif_blocks else_block) ctx
    | StatementApply apply -> exec_expr (ExprApply apply) ctx
    | Return ex -> exec_return ex ctx
    | FunctionDeclare (id, ids, blk) -> exec_set_one id (Const.Function (ids, blk)) ctx
    | Break -> exec_break ctx
    | While (ex, blck) -> exec_loop_ctx (exec_while ex blck) ctx
    | Repeat (blck, ex) -> exec_loop_ctx (exec_until blck ex) ctx
    | Fornum (ident, st, en, step, blck) ->
      exec_loop_ctx (exec_for_num ident st en step blck) ctx
    | Forin (_, _, _) -> error "For in loop is not implemented yet =( " ctx

  and exec_for_num id bg ed st blck =
    exec_expr bg
    >>= fun loop_begin ->
    exec_expr ed
    >>= fun loop_end ->
    exec_expr
      (match st with
       | Some s -> s
       | None -> Const (Number 1.))
    >>= fun loop_step ->
    match loop_begin.last_exec, loop_end.last_exec, loop_step.last_exec with
    | Number lbegin, Number lend, Number lstep ->
      let rec loop_iter iter ctx =
        match
          (if lstep > 0. then ( <= ) else ( >= )) (lbegin +. (iter *. lstep)) lend
        with
        | false -> return ctx
        | true ->
          exec_local_set [ id ] [ Const (Number (lbegin +. (iter *. lstep))) ]
          >>= fun _ -> exec_many blck >>= fun nctx -> loop_iter (iter +. 1.) nctx
      in
      loop_iter 0. loop_step
    | _, _, _ -> error "End, begin and step of the loop must be numbers"

  and exec_while ex blck =
    exec_expr ex
    >>= fun ctx_e ->
    match get_bool ctx_e.last_exec with
    | true -> exec_many blck >>= fun _ -> exec_while ex blck
    | false -> return ctx_e

  and exec_until blck ex =
    exec_many blck
    >>= fun _ ->
    exec_expr ex
    >>= fun ctx_e ->
    match get_bool ctx_e.last_exec with
    | true -> exec_until blck ex
    | false -> return ctx_e

  and exec_break ctx =
    let rec find_ctx lvl c =
      if c.level = lvl
      then Breaking c
      else if c.level < lvl
      then Error "Breaking outside a loop"
      else (
        match c.previous with
        | Some p -> find_ctx lvl p
        | None -> Error "Trying to exit global context")
    in
    match ctx.brk with
    | Some lvl -> find_ctx lvl ctx
    | None -> Error "Breaking outside a loop"

  and exec_return ex =
    exec_expr
      (match ex with
       | Some e -> e
       | None -> Const Nil)
    >>= fun ctx ->
    let rec find_ctx lvl c =
      if c.level = lvl
      then Returning { c with last_exec = ctx.last_exec }
      else if c.level < lvl
      then Error "Returning outside a function"
      else (
        match c.previous with
        | Some p -> find_ctx lvl p
        | None -> Error "Trying to exit global context")
    in
    match ctx.ret with
    | Some lvl -> fun _ -> find_ctx lvl ctx
    | None -> error "Returning outside a function"

  and get_bool = function
    | Bool false | Nil -> false
    | _ -> true

  and exec_if iex blck elseifs elseblck =
    exec_expr iex
    >>= fun if_ex_res ->
    match get_bool if_ex_res.last_exec with
    | true -> exec_many blck
    | _ ->
      (match elseifs with
       | [] ->
         (match elseblck with
          | Some sttmnts -> exec_many sttmnts
          | None -> exec_many [])
       | h :: t ->
         (match h with
          | iex, blck -> exec_if iex blck t elseblck))

  and exec_set ce re ctx =
    let rec helper cl rl ctx =
      match cl, rl with
      | ch :: ct, rh :: rt ->
        exec_expr rh
        >>= fun rhr -> exec_set_one ch rhr.last_exec >>= fun nctx -> helper ct rt nctx
      | ch :: ct, [] -> exec_set_one ch Nil >>= fun nctx -> helper ct rl nctx
      | [], _ -> return ctx
    in
    helper ce re ctx ctx

  and exec_set_one ce re ctx =
    let rec replace_ident i ctx =
      match ctx.previous with
      | None -> { ctx with vars = VarsMap.replace i re ctx.vars }
      | Some prev ->
        (match VarsMap.find_opt i ctx.vars with
         | None -> { ctx with previous = Some (replace_ident i prev) }
         | Some _ -> { ctx with vars = VarsMap.replace i re ctx.vars })
    in
    let rec apply_to_ctx f lval ctx =
      match lval with
      | Ident i ->
        (match VarsMap.find_opt i ctx.vars with
         | Some _ -> f ctx
         | None ->
           (match ctx.previous with
            | Some prev ->
              (match apply_to_ctx f (Ident i) prev with
               | Interpreted nctx -> Interpreted { ctx with previous = Some nctx }
               | x -> x)
            | None -> Error "Attempting to index nil"))
      | Index (p, _) -> apply_to_ctx f p ctx
    in
    let rec indexes_to_list index ctx =
      match index with
      | Index (p, i) ->
        (match exec_expr i ctx with
         | Error m -> "", [], Error m
         | Interpreted nctx ->
           let next_res = indexes_to_list p nctx in
           let e1, e2, e3 = next_res in
           e1, nctx.last_exec :: e2, e3
         | _ -> "", [], Error "Breaking or returning still..")
      | Ident i -> i, [], Interpreted ctx
    in
    let replace_index index ctx =
      let primal_id, const_lst, ctx = indexes_to_list index ctx in
      let rec replace_in_table klst tbl =
        match klst with
        | h :: [] -> Some (TableMap.replace h re tbl)
        | h :: t ->
          (match TableMap.find_opt h tbl with
           | Some (Table ntbl) ->
             (match replace_in_table t ntbl with
              | Some nntbl -> Some (TableMap.replace h (Table nntbl) tbl)
              | None -> None)
           | _ -> None)
        | [] -> Some tbl
      in
      let f ctx =
        match VarsMap.find_opt primal_id ctx.vars with
        | Some tbl ->
          (match tbl with
           | Table tbl ->
             (match replace_in_table (List.rev const_lst) tbl with
              | Some tbl ->
                Interpreted
                  { ctx with vars = VarsMap.replace primal_id (Table tbl) ctx.vars }
              | None -> Error "Attempt to index non table")
           | _ -> Error "Attempt to index non table")
        | None -> Error "Attempt to index nil"
      in
      match ctx with
      | Interpreted ctx -> apply_to_ctx f (Ident primal_id) ctx
      | e -> e
    in
    match ce with
    | Ident ident -> Interpreted (replace_ident ident ctx)
    | index -> replace_index index ctx

  and exec_many stmnts ctx =
    match stmnts with
    | [] -> Interpreted ctx
    | h :: t -> (exec_stat h >>= fun _ -> exec_many t) ctx
  ;;

  let emptyctx =
    { vars = VarsMap.empty
    ; previous = None
    ; last_exec = Nil
    ; ret = None
    ; brk = None
    ; level = 0
    }
  ;;

  let interpret ast ctx =
    match exec_many ast ctx with
    | Error m -> Fail m
    | Interpreted m -> Done m
    | Breaking _ -> Fail "Breaking outside a loop"
    | Returning _ -> Fail "Returning outside a function"
  ;;
end
