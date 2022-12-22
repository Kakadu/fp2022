(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Ident
open Pass

exception RuntimeExn of string

let runtime_exn msg = raise (RuntimeExn msg)

type value =
  | VInt of int
  | VBool of bool
  | VStr of string
  | VArr of value list
  | VFunc of vfunc
  | VVoid

and vfunc = ident signature * ident block

and env =
  { parent : env option
  ; tbl : value ref tbl
  }

type eval_state =
  { env : env
  ; returned : value option
  }

module P = Pass (struct
  type t = eval_state
end)

open P

let push_fn =
  let* s = access in
  let env = { parent = Some s.env; tbl = empty_tbl } in
  put { s with env }
;;

let pop_fn =
  let* s = access in
  let env =
    match s.env with
    | { parent = None; _ } -> failwith "Cannot pop global scope"
    | { parent = Some env; _ } -> env
  in
  put { s with env }
;;

let new_var id value =
  let* s = access in
  match s with
  | { env = { parent; tbl }; returned } ->
    let tbl = Ident.set tbl ~key:id ~data:(ref value) in
    put { env = { parent; tbl }; returned }
;;

let set_var id value =
  let rec set_ref env =
    let { parent; tbl } = env in
    let var = Ident.find tbl id in
    match parent, var with
    | _, Some var -> var := value
    | Some env, None -> set_ref env
    | None, None -> failwith "Variable not found!"
  in
  let* s = access in
  set_ref s.env;
  put s
;;

let get_var id =
  let rec get_ref_val env =
    let { parent; tbl } = env in
    let var = Ident.find tbl id in
    match parent, var with
    | _, Some var -> !var
    | Some env, None -> get_ref_val env
    | None, None -> failwith "Variable not found!"
  in
  let* s = access in
  return (get_ref_val s.env)
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

let rec value_to_string = function
  | VInt i -> Int.to_string i
  | VBool b -> Bool.to_string b
  | VStr s -> s
  | VArr arr ->
    let els = List.map arr ~f:value_to_string in
    "[" ^ String.concat ~sep:", " els ^ "]"
  | VFunc (sign, _) ->
    "func" ^ show_typ (FunTyp (ident_sign_to_string_sign sign)) ^ "{ ... }"
  | VVoid -> "void"
;;

(* Expressions *)

let rec eval_expr = function
  | Const (Int x) -> return (VInt x)
  | Const (Str x) -> return (VStr x)
  | Const (Bool x) -> return (VBool x)
  | Ident id -> get_var id
  | ArrLit (_, els) -> eval_arr_lit els
  | ArrIndex (arr, i) -> eval_arr_index arr i
  | Call (f, args) ->
    let* f = eval_expr f in
    let* args = many args ~f:eval_expr in
    eval_call f args
  | FuncLit (sign, b) -> eval_func_lit sign b
  | UnOp (op, e) -> eval_unop op e
  | BinOp (l, op, r) -> eval_binop l op r
  | Print args -> eval_print args

and eval_arr_lit els =
  let* els = many els ~f:eval_expr in
  return (VArr els)

and eval_arr_index arr i =
  let* arr = eval_expr arr in
  let* i = eval_expr i in
  match arr, i with
  | VArr arr, VInt i ->
    (match List.nth arr i with
     | Some x -> return x
     | None -> failwith "Usererror: array index out of bounds")
  | _ -> failwith "Internal error: illegal types"

and eval_call f args =
  let set_args formal_args args =
    let formal_args = List.map formal_args ~f:(fun (id, _) -> id) in
    match List.zip formal_args args with
    | List.Or_unequal_lengths.Ok lst ->
      fold_state lst ~f:(fun (farg, arg) -> new_var farg arg)
    | List.Or_unequal_lengths.Unequal_lengths ->
      failwith "Internal error: illegal number of args"
  in
  match f with
  | VFunc ({ args = fargs; _ }, b) ->
    let* _ = push_fn in
    let* _ = set_args fargs args in
    let* _ = eval_block b in
    let* _ = pop_fn in
    let* r = remove_returned in
    (match r with
     | Some r -> return r
     | None -> return VVoid)
  | _ -> failwith "Internal error: illegal types"

and eval_func_lit sign block = return (VFunc (sign, block))

and eval_unop op e =
  let* e = eval_expr e in
  match op, e with
  | Minus, VInt i -> return (VInt (-i)) (* Todo this is dumb *)
  | Not, VBool b -> return (VBool (Bool.equal b false))
  | _ -> failwith "Internal error: Illegal type"

and eval_binop l op r =
  let* l = eval_expr l in
  let* r = eval_expr r in
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
    | _ -> failwith "Illegal binary op"
  in
  return res

and eval_print args =
  let* args = many args ~f:eval_expr in
  let args = List.map args ~f:value_to_string in
  let str = String.concat ~sep:" " args in
  Caml.print_string str;
  return VVoid

(* Statements *)

and eval_stmt stmt =
  let* r = get_returned in
  match r with
  | Some _ -> return ()
  | None ->
    (match stmt with
     | AssignStmt (l, r) -> eval_assign l r
     | VarDecl v -> eval_vardecl v
     | BlockStmt b -> eval_block b
     | ExprStmt e -> eval_expr e *> return ()
     | RetStmt (Some v) ->
       let* v = eval_expr v in
       set_returned v
     | RetStmt None -> set_returned VVoid
     | IfStmt (cond, b1, b2) -> eval_if cond b1 b2
     | _ -> assert false)

and eval_block b = fold_state b ~f:eval_stmt

and eval_assign l r =
  let* r = eval_expr r in
  match l with
  | Ident id -> set_var id r
  | ArrIndex (arr, i) -> eval_arr_index_assign arr i r
  | _ -> failwith "Illegal assignment"

and eval_arr_index_assign receiver i x =
  (* Evaluates receiver[i] = x *)
  let list_set_i list i x =
    if 0 <= i && i < List.length list
    then List.mapi list ~f:(fun j el -> if i = j then x else el)
    else runtime_exn "Array index out of bounds"
  in
  let* i = eval_expr i in
  let* list = eval_expr receiver in
  let new_list =
    match list, i with
    | VArr list, VInt i -> VArr (list_set_i list i x)
    | _ -> failwith "Internal error: typing"
  in
  match receiver with
  | Ident id -> set_var id new_list
  | ArrIndex (receiver, new_i) -> eval_arr_index_assign receiver new_i new_list
  | _ -> return ()

and eval_vardecl (id, expr) =
  let* value = eval_expr expr in
  new_var id value

and eval_if cond bthen belse =
  let* cond = eval_expr cond in
  match cond with
  | VBool true -> eval_block bthen
  | VBool false -> eval_block belse
  | _ -> failwith "Internal error: illegal types"
;;

let eval_file file =
  (* Define globals *)
  let vars =
    List.filter_map file ~f:(fun x ->
      match x with
      | GlobalVarDecl v -> Some v
      | _ -> None)
  in
  let* _ = fold_state vars ~f:eval_vardecl in
  (* Define functions *)
  let funcs =
    List.filter_map file ~f:(fun x ->
      match x with
      | FuncDecl f -> Some f
      | _ -> None)
  in
  let eval_func_decl (id, sign, b) =
    let* v = eval_func_lit sign b in
      new_var id v
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
  let _ =
    run_pass
      (eval_file file)
      ~init:{ env = { parent = None; tbl = empty_tbl }; returned = None }
  in
  ()
;;
