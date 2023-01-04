(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Ident

type found_decl =
  | Var of ident var_decl
  | Func of ident func_decl
  | Arg of ident arg

type find_decl_state =
  { target : ident
  ; found : found_decl list
  }

module P = Pass.Pass (struct
  type t = find_decl_state
end)

open P

let check_decl d =
  let* s = access in
  let id =
    match d with
    | Var (id, _) | Func (id, _, _) | Arg (id, _) -> id
  in
  if eq_ident id s.target then put { s with found = d :: s.found } else return ()
;;

let rec findin_exprs exprs = fold_state exprs ~f:findin_expr

and findin_expr = function
  | Const _ | Ident _ | Make _ -> return ()
  | ArrLit (_, exprs) | Print exprs -> findin_exprs exprs
  | ArrIndex (l, r) | BinOp (l, _, r) -> findin_exprs [ l; r ]
  | Call (f, args) -> findin_exprs (f :: args)
  | UnOp (_, e) -> findin_expr e
  | FuncLit (s, b) -> findin_sign s *> findin_block b
  | Print xs -> findin_exprs xs
  | Len x -> findin_expr x
  | Append (arr, vs) -> findin_expr arr *> findin_exprs vs

and findin_sign { args; _ } =
  let findin_arg arg = check_decl (Arg arg) in
  fold_state args ~f:findin_arg

and findin_block b = fold_state b ~f:findin_stmt

and findin_stmt = function
  | AssignStmt (l, r) | SendStmt (l, r) -> findin_exprs [ l; r ]
  | VarDecl v -> check_decl (Var v)
  | BlockStmt b -> findin_block b
  | GoStmt e | ExprStmt e -> findin_expr e
  | RetStmt e ->
    (match e with
     | Some e -> findin_expr e
     | None -> return ())
  | IfStmt (cond, b1, b2) ->
    let* _ = findin_expr cond in
    let* _ = findin_block b1 in
    findin_block b2
  | ForStmt (cond, b) ->
    let* _ = findin_expr cond in
    findin_block b
;;

let findin_toplevel = function
  | GlobalVarDecl v -> check_decl (Var v)
  | FuncDecl (id, sign, b) ->
    let* _ = check_decl (Func (id, sign, b)) in
    let* _ = findin_sign sign in
    findin_block b
;;

let findin_file file = fold_state file ~f:findin_toplevel

let find f id =
  let { found; _ }, _ = run_pass (findin_file f) ~init:{ target = id; found = [] } in
  match found with
  | [] -> None
  | d :: _ -> Some d
;;
