(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Ident

type state = string list

module P = Pass.Pass (struct
  type t = state
end)

open P

let add_err msg =
  let* s = access in
  put (msg :: s)
;;

let rec pass_expr e =
  match e with
  | Call (Ident id, args) -> pass_id_call id args
  | ArrLit (t, els) ->
    let* exprs = pass_exprs els in
    return (ArrLit (t, exprs))
  | ArrIndex (a, i) ->
    let* a = pass_expr a in
    let* i = pass_expr i in
    return (ArrIndex (a, i))
  | Call (f, args) ->
    let* f = pass_expr f in
    let* args = pass_exprs args in
    return (Call (f, args))
  | FuncLit (s, b) ->
    let* b = pass_block b in
    return (FuncLit (s, b))
  | UnOp (op, e) ->
    let* e = pass_expr e in
    return (UnOp (op, e))
  | BinOp (l, op, r) ->
    let* l = pass_expr l in
    let* r = pass_expr r in
    return (BinOp (l, op, r))
  | e -> return e

and pass_exprs exprs = many exprs ~f:pass_expr

and pass_id_call id args =
  let* args = pass_exprs args in
  match as_builtin id with
  | Some name ->
    let* builtin = map_builtin name args in
    (match builtin with
     | Some x -> return x
     | _ -> return (Call (Ident id, args)))
  | _ -> return (Call (Ident id, args))

and map_builtin name args =
  let* args = pass_exprs args in
  match name with
  | "print" -> return (Some (Print args))
  | "append" ->
    (match args with
     | arr :: first :: rest -> return (Some (Append (arr, first :: rest)))
     | _ -> add_err "append() takes at least 2 arguments" *> return None)
  | "len" ->
    (match args with
     | [ x ] -> return (Some (Len x))
     | _ -> add_err "len() built-in takes exactly 1 argument" *> return None)
  | _ -> failwith "internal error"

and pass_stmt = function
  | AssignStmt (l, r) ->
    let* l = pass_expr l in
    let* r = pass_expr r in
    return (AssignStmt (l, r))
  | VarDecl (name, r) ->
    let* r = pass_expr r in
    return (VarDecl (name, r))
  | BlockStmt b ->
    let* b = pass_block b in
    return (BlockStmt b)
  | ExprStmt e ->
    let* e = pass_expr e in
    return (ExprStmt e)
  | GoStmt e ->
    let* e = pass_expr e in
    return (GoStmt e)
  | RetStmt e ->
    (match e with
     | Some e ->
       let* e = pass_expr e in
       return (RetStmt (Some e))
     | _ -> return (RetStmt None))
  | IfStmt (cond, b1, b2) ->
    let* cond = pass_expr cond in
    let* b1 = pass_block b1 in
    let* b2 = pass_block b2 in
    return (IfStmt (cond, b1, b2))
  | ForStmt (cond, b) ->
    let* cond = pass_expr cond in
    let* b = pass_block b in
    return (ForStmt (cond, b))

and pass_block stmts = many stmts ~f:pass_stmt

let pass_toplevel = function
  | GlobalVarDecl (name, e) ->
    let* e = pass_expr e in
    return (GlobalVarDecl (name, e))
  | FuncDecl (name, s, b) ->
    let* b = pass_block b in
    return (FuncDecl (name, s, b))
;;

let pass_file (f : ident source_file) = many f ~f:pass_toplevel

let pass f =
  let s, f = run_pass (pass_file f) ~init:[] in
  match s with
  | [] -> Ok f
  | errs -> Error errs
;;
