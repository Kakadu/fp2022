(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast

type state = string list

let add_err s msg = msg :: s

let rec pass_stmt s = function
  | RetStmt _ -> s, true
  | VarDecl (_, FuncLit (sign, block)) -> check_fn s "anonymous" sign block, false
  | IfStmt (_, b1, b2) ->
    let s, res1 = pass_stmts s b1 in
    let s, res2 = pass_stmts s b2 in
    s, res1 && res2
  | _ -> s, false

and pass_stmts s stmts =
  let folder acc stmt =
    let s, res = acc in
    let s, res1 = pass_stmt s stmt in
    s, res || res1
  in
  List.fold stmts ~init:(s, false) ~f:folder

and check_fn s name { ret; _ } block =
  let s, terminates = pass_stmts s block in
  match ret with
  | One _ ->
    if terminates then s else add_err s ("Missing return statement in function " ^ name)
  | Void -> s
;;

let check_toplevel s = function
  | GlobalVarDecl _ -> s
  | FuncDecl (id, sign, block) -> check_fn s (Ident.name id) sign block
;;

let check_file file =
  let initial_state = [] in
  let s = List.fold file ~init:initial_state ~f:check_toplevel in
  match s with
  | [] -> Ok ()
  | _ -> Error s
;;
