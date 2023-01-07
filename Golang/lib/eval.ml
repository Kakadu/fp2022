(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Ident
open Pass

exception InternalExn

type value =
  | VInt of int
  | VBool of bool
  | VStr of string
  | VArr of value list
  | VFunc of vfunc
  | VChan of value Channel.t
  | VVoid

and func_env =
  | Closure of env
  | NoClosure

and vfunc = func_env * ident signature * ident block

and env =
  { parent : env option
  ; tbl : value tbl
  }

type eval_state =
  { env : env
  ; returned : value option
  ; err : string option
  }

module P = Pass (struct
  type t = eval_state
end)

open P

let set_env env =
  let* s = access in
  let* _ = put { s with env } in
  return s.env
;;

let get_env =
  let* s = access in
  return s.env
;;

let push_fn =
  let* s = access in
  let env = { parent = Some s.env; tbl = empty_tbl } in
  put { s with env }
;;

let enter_func_call closure_env =
  let* cur_env =
    match closure_env with
    | Closure closure_env -> set_env closure_env
    | NoClosure -> get_env
  in
  let* _ = push_fn in
  return cur_env
;;

let exit_func_call env =
  let* _ = set_env env in
  return ()
;;

let new_var id value =
  let* s = access in
  match s.env with
  | { parent; tbl } ->
    let tbl = Ident.set tbl ~key:id ~data:value in
    put { s with env = { parent; tbl } }
;;

let set_var id value =
  let rec set { parent; tbl } =
    let var = Ident.find tbl id in
    match parent, var with
    | _, Some _ -> { parent; tbl = Ident.set tbl ~key:id ~data:value }
    | Some parent, None -> { parent = Some (set parent); tbl }
    | None, None -> raise InternalExn
  in
  let* s = access in
  let env = set s.env in
  put { s with env }
;;

let get_var id =
  let rec get_val env =
    let { parent; tbl } = env in
    let var = Ident.find tbl id in
    match parent, var with
    | _, Some var -> var
    | Some env, None -> get_val env
    | None, None -> raise InternalExn
  in
  let* s = access in
  return (get_val s.env)
;;

let set_returned value =
  let* s = access in
  put { s with returned = Some value }
;;

let remove_returned =
  let* s = access in
  let r = s.returned in
  let* _ = put { s with returned = None } in
  return r
;;

let get_returned =
  let* s = access in
  return s.returned
;;

let set_err err =
  let* s = access in
  match s.err with
  | Some _ -> return ()
  | None -> put { s with err = Some err }
;;

let should_eval_stmt =
  let* s = access in
  match s.returned, s.err with
  | None, None -> return true
  | _ -> return false
;;

let array_index_out_of_bounds = return (Error "Array index out of bounds")

(* Custom binds *)

type 'v expr_t = ('v, string) result t
type stmt_t = unit t

(* This bind should be used to eval expressions inside statements *)
let stmt_bind (t : 'v expr_t) (f : 'v -> stmt_t) : stmt_t =
  let* res = t in
  match res with
  | Ok v -> f v
  | Error err -> set_err err
;;

let ( let+ ) = stmt_bind

(* This bind should be used to eval expressions inside expressions *)
let expr_bind (t : 'a expr_t) (f : 'a -> 'b expr_t) : ('b, string) result t =
  let* res = t in
  match res with
  | Ok a -> f a
  | Error s -> return (Error s)
;;

let ( let** ) = expr_bind

let many_exprs (exprs : 'a list) ~(f : 'a -> 'b expr_t) : 'b list expr_t =
  let rec helper acc xs =
    let* ys = acc in
    match xs with
    | [] -> return (Ok (List.rev ys))
    | h :: tl ->
      let** y = f h in
      helper (return (y :: ys)) tl
  in
  helper (return []) exprs
;;

let rec value_to_string = function
  | VInt i -> Int.to_string i
  | VBool b -> Bool.to_string b
  | VStr s -> s
  | VArr arr ->
    let els = List.map arr ~f:value_to_string in
    "[" ^ String.concat ~sep:", " els ^ "]"
  | VFunc (_, sign, _) ->
    "func" ^ show_typ (FunTyp (ident_sign_to_string_sign sign)) ^ "{ ... }"
  | VVoid -> "void"
  | VChan _ -> "chan"
;;

(* Expressions *)

let rec eval_expr = function
  | Const (Int x) -> return (Ok (VInt x))
  | Const (Str x) -> return (Ok (VStr x))
  | Const (Bool x) -> return (Ok (VBool x))
  | Ident id ->
    let* v = get_var id in
    return (Ok v)
  | ArrLit (_, els) -> eval_arr_lit els
  | ArrIndex (arr, i) -> eval_arr_index arr i
  | Call (f, args) ->
    let** f = eval_expr f in
    let** args = many_exprs args ~f:eval_expr in
    eval_call f args
  | FuncLit (sign, b) ->
    let* env = get_env in
    eval_func_lit (Closure env) sign b
  | UnOp (op, e) -> eval_unop op e
  | BinOp (l, op, r) -> eval_binop l op r
  | Print args -> eval_print args
  | Len e -> eval_len e
  | Append (arr, vs) -> eval_append arr vs
  | Make t -> eval_make t

and eval_exprs exprs = many_exprs exprs ~f:eval_expr

and eval_arr_lit els =
  let** els = eval_exprs els in
  return (Ok (VArr els))

and eval_arr_index arr i =
  let** arr = eval_expr arr in
  let** i = eval_expr i in
  match arr, i with
  | VArr arr, VInt i ->
    (match List.nth arr i with
     | Some x -> return (Ok x)
     | None -> array_index_out_of_bounds)
  | _ -> raise InternalExn

and eval_call f args =
  let set_args formal_args args =
    let formal_args = List.map formal_args ~f:(fun (id, _) -> id) in
    match List.zip formal_args args with
    | List.Or_unequal_lengths.Ok lst ->
      fold_state lst ~f:(fun (farg, arg) -> new_var farg arg)
    | List.Or_unequal_lengths.Unequal_lengths -> raise InternalExn
  in
  match f with
  | VFunc (closure_env, { args = fargs; _ }, b) ->
    let* env = enter_func_call closure_env in
    let* _ = set_args fargs args in
    let* _ = eval_block b in
    let* _ = exit_func_call env in
    let* r = remove_returned in
    (match r with
     | Some r -> return (Ok r)
     | None -> return (Ok VVoid))
  | _ -> raise InternalExn

and eval_func_lit env sign block = return (Ok (VFunc (env, sign, block)))

and eval_unop op e =
  let** e = eval_expr e in
  match op, e with
  | Minus, VInt i -> return (Ok (VInt (-i)))
  | Not, VBool b -> return (Ok (VBool (Bool.equal b false)))
  | Receive, VChan c -> return (Ok (Channel.receive c))
  | _ -> raise InternalExn

and eval_binop l op r =
  let** l = eval_expr l in
  let** r = eval_expr r in
  let res =
    match l, op, r with
    | VInt a, Mul, VInt b -> VInt (a * b)
    | VInt a, Div, VInt b -> VInt (a / b)
    | VInt a, Mod, VInt b -> VInt (a % b)
    | VInt a, Add, VInt b -> VInt (a + b)
    | VInt a, Sub, VInt b -> VInt (a - b)
    | VInt a, Eq, VInt b -> VBool (a = b)
    | VInt a, Neq, VInt b -> VBool (not (phys_equal a b))
    | VInt a, Lt, VInt b -> VBool (a < b)
    | VInt a, Lte, VInt b -> VBool (a <= b)
    | VInt a, Gte, VInt b -> VBool (a >= b)
    | VInt a, Gt, VInt b -> VBool (a > b)
    | VStr a, Add, VStr b -> VStr (a ^ b)
    | VBool a, And, VBool b -> VBool (a && b)
    | VBool a, Or, VBool b -> VBool (a || b)
    | _ -> raise InternalExn
  in
  return (Ok res)

and eval_print args =
  let** args = eval_exprs args in
  let args = List.map args ~f:value_to_string in
  let str = String.concat ~sep:" " args in
  Caml.Printf.fprintf Caml.stdout "%s" str;
  Caml.Printf.fprintf Caml.stdout "%!";
  return (Ok VVoid)

and eval_len e =
  let** arr = eval_expr e in
  match arr with
  | VArr list -> return (Ok (VInt (List.length list)))
  | _ -> raise InternalExn

and eval_append arr vs =
  let** arr = eval_expr arr in
  let** vs = eval_exprs vs in
  match arr with
  | VArr list -> return (Ok (VArr (List.append list vs)))
  | _ -> raise InternalExn

and eval_make typ =
  let res =
    match typ with
    | IntTyp -> VInt 0
    | StrTyp -> VStr ""
    | BoolTyp -> VBool false
    | ArrayTyp _ -> VArr []
    | ChanTyp _ -> VChan (Channel.create ())
    | _ -> raise InternalExn
  in
  return (Ok res)

(* Statements *)

and eval_stmt stmt =
  let* should_eval = should_eval_stmt in
  if should_eval
  then (
    match stmt with
    | AssignStmt (l, r) -> eval_assign l r
    | VarDecl v -> eval_vardecl v
    | BlockStmt b -> eval_block b
    | ExprStmt e -> eval_expr e *> return ()
    | RetStmt (Some v) ->
      let+ v = eval_expr v in
      set_returned v
    | RetStmt None -> set_returned VVoid
    | IfStmt (cond, b1, b2) -> eval_if cond b1 b2
    | ForStmt (cond, b) -> eval_for cond b
    | GoStmt e -> eval_go e
    | SendStmt (chan, x) -> eval_send chan x)
  else return ()

and eval_block b = fold_state b ~f:eval_stmt

and eval_assign l r =
  let+ r = eval_expr r in
  match l with
  | Ident id -> set_var id r
  | ArrIndex (arr, i) -> eval_arr_index_assign arr i r
  | _ -> raise InternalExn

and eval_arr_index_assign receiver i x =
  (* Evaluates receiver[i] = x *)
  let+ i = eval_expr i in
  let+ list = eval_expr receiver in
  let+ new_list =
    match list, i with
    | VArr list, VInt i ->
      if 0 <= i && i < List.length list
      then (
        let newlist = List.mapi list ~f:(fun j el -> if i = j then x else el) in
        return (Ok (VArr newlist)))
      else array_index_out_of_bounds
    | _ -> raise InternalExn
  in
  match receiver with
  | Ident id -> set_var id new_list
  | ArrIndex (receiver, new_i) -> eval_arr_index_assign receiver new_i new_list
  | _ -> return ()

and eval_vardecl (id, expr) =
  let+ value = eval_expr expr in
  new_var id value

and eval_go expr =
  let* s = access in
  let run _ = run_pass (eval_expr expr) ~init:s in
  let _ = Thread.create run () in
  return ()

and eval_if cond bthen belse =
  let+ cond = eval_expr cond in
  match cond with
  | VBool true -> eval_block bthen
  | VBool false -> eval_block belse
  | _ -> raise InternalExn

and eval_for cond body =
  let+ value = eval_expr cond in
  match value with
  | VBool true ->
    eval_block body *> eval_stmt (ForStmt (cond, body)) (* reuse "return" logic*)
  | VBool false -> return ()
  | _ -> raise InternalExn

and eval_send chan x =
  let+ chan = eval_expr chan in
  let+ x = eval_expr x in
  match chan with
  | VChan chan ->
    Channel.send chan x;
    return ()
  | _ -> raise InternalExn
;;

let eval_file file =
  (* Define globals *)
  let vars =
    List.filter_map file ~f:(function
      | GlobalVarDecl v -> Some v
      | _ -> None)
  in
  let* _ = fold_state vars ~f:eval_vardecl in
  (* Define functions *)
  let funcs =
    List.filter_map file ~f:(function
      | FuncDecl f -> Some f
      | _ -> None)
  in
  let eval_func_decl (id, sign, b) =
    let* _ = new_var id VVoid in
    let+ v = eval_func_lit NoClosure sign b in
    set_var id v
  in
  let* _ = fold_state funcs ~f:eval_func_decl in
  (* Eval main() *)
  let eval_main = function
    | FuncDecl (id, _, b) when String.equal (name id) "main" -> eval_block b
    | _ -> return ()
  in
  fold_state file ~f:eval_main
;;

let eval file =
  let s, () =
    run_pass
      (eval_file file)
      ~init:{ env = { parent = None; tbl = empty_tbl }; returned = None; err = None }
  in
  match s.err with
  | None -> Ok ()
  | Some err -> Error err
;;
