(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Ident

module Scope = struct
  type t =
    { id : int
    ; parent : t option
    ; tab : (string, ident, String.comparator_witness) Map.t
    }

  type declaration_result =
    | Ok of ident * t
    | Already_declared

  let empty_global_scope (builtins : ident list) =
    let tab = Map.empty (module String) in
    let tab =
      List.fold builtins ~init:tab ~f:(fun tab x ->
        Map.set tab ~key:(Ident.name x) ~data:x)
    in
    { id = 0; parent = None; tab }
  ;;

  let empty_local_scope p id = { id; parent = Some p; tab = Map.empty (module String) }
  let lookup_immediate scope name = Map.find scope.tab name

  let rec resolve scope name =
    match lookup_immediate scope name with
    | Some d -> Some d
    | _ ->
      (match scope.parent with
       | None -> None
       | Some parent -> resolve parent name)
  ;;

  let declare scope name =
    match lookup_immediate scope name with
    | Some _ -> Already_declared
    | None ->
      let id = ident ~scope:scope.id name in
      Ok (id, { scope with tab = Map.set scope.tab ~key:name ~data:id })
  ;;
end

type lookup_state =
  { next_id : int
  ; scope : Scope.t
  ; errs : string list
  }

module P = Pass.Pass (struct
  type t = lookup_state
end)

open P

let initial_state builtins =
  let builtins = List.map builtins ~f:Ident.builtin in
  let scope = Scope.empty_global_scope builtins in
  { next_id = 1; scope; errs = [] }
;;

let add_err s : unit t =
  let* state = access in
  let state = { state with errs = s :: state.errs } in
  put state
;;

let declare (name : string) : ident t =
  let* state = access in
  match Scope.declare state.scope name with
  | Ok (id, scope) ->
    let* _ = put { state with scope } in
    return id
  | Already_declared ->
    let* _ = add_err ("Identifier " ^ name ^ " already declared in this scope") in
    return error_ident
;;

let resolve (name : string) : ident t =
  let* state = access in
  match Scope.resolve state.scope name with
  | Some id -> return id
  | None ->
    let* _ = add_err ("Identifier " ^ name ^ " is not declared!") in
    return error_ident
;;

let enter_scope : Scope.t t =
  let* state = access in
  let id = state.next_id in
  let old_scope = state.scope in
  let scope = Scope.empty_local_scope old_scope id in
  let* _ = put { state with next_id = id + 1; scope } in
  return old_scope
;;

let set_scope scope : unit t =
  let* state = access in
  put { state with scope }
;;

(* Expressions *)
let rec lookup_expr = function
  | Const c -> return (Const c)
  | Ident name ->
    let* id = resolve name in
    return (Ident id)
  | ArrLit (t, exprs) ->
    let* exprs = lookup_exprs exprs in
    return (ArrLit (t, exprs))
  | ArrIndex (arr, i) ->
    let* arr = lookup_expr arr in
    let* i = lookup_expr i in
    return (ArrIndex (arr, i))
  | Call (f, args) ->
    let* f = lookup_expr f in
    let* args = lookup_exprs args in
    return (Call (f, args))
  | FuncLit (sign, b) ->
    let* sign, b = lookup_func sign b in
    return (FuncLit (sign, b))
  | UnOp (op, e) ->
    let* e = lookup_expr e in
    return (UnOp (op, e))
  | BinOp (l, op, r) ->
    let* l = lookup_expr l in
    let* r = lookup_expr r in
    return (BinOp (l, op, r))
  | Print e ->
    let* e = lookup_exprs e in
    return (Print e)
  | Len e ->
    let* e = lookup_expr e in
    return (Len e)

and lookup_func sign b =
  let* enclosing_scope = enter_scope in
  let* sign = lookup_declare_signature sign in
  let* b = lookup_block b in
  let* _ = set_scope enclosing_scope in
  return (sign, b)

and lookup_declare_signature { args; ret } =
  let lookup_declare_arg (name, t) =
    let* id = declare name in
    return (id, t)
  in
  let* args = many args ~f:lookup_declare_arg in
  return { args; ret }

and lookup_exprs exprs = many exprs ~f:lookup_expr

(* Statements *)

and lookup_stmt = function
  | AssignStmt (l, r) ->
    let* l = lookup_expr l in
    let* r = lookup_expr r in
    return (AssignStmt (l, r))
  | VarDecl (name, expr) ->
    let* id, expr = lookup_vardecl name expr in
    return (VarDecl (id, expr))
  | BlockStmt b ->
    let* b = lookup_block b in
    return (BlockStmt b)
  | ExprStmt e ->
    let* e = lookup_expr e in
    return (ExprStmt e)
  | GoStmt e ->
    let* e = lookup_expr e in
    return (GoStmt e)
  | RetStmt e ->
    (match e with
     | Some e ->
       let* e = lookup_expr e in
       return (RetStmt (Some e))
     | None -> return (RetStmt None))
  | IfStmt (cond, b1, b2) ->
    let* cond = lookup_expr cond in
    let* b1 = lookup_block b1 in
    let* b2 = lookup_block b2 in
    return (IfStmt (cond, b1, b2))
  | ForStmt (cond, b) ->
    let* cond = lookup_expr cond in
    let* b = lookup_block b in
    return (ForStmt (cond, b))

and lookup_vardecl name expr =
  let* expr = lookup_expr expr in
  let* id = declare name in
  return (id, expr)

and lookup_block b =
  let* enclosing_scope = enter_scope in
  let* stmts = many b ~f:lookup_stmt in
  let* _ = set_scope enclosing_scope in
  return stmts
;;

let lookup_file file =
  let instantly_declare_toplevel = function
    | GlobalVarDecl (name, _) -> declare name *> return ()
    | FuncDecl (name, _, _) -> declare name *> return ()
  in
  let lookup_nodeclare_toplevel = function
    | GlobalVarDecl (name, e) ->
      let* id = resolve name in
      let* e = lookup_expr e in
      return (GlobalVarDecl (id, e))
    | FuncDecl (name, sign, b) ->
      let* id = resolve name in
      let* sign, b = lookup_func sign b in
      return (FuncDecl (id, sign, b))
  in
  let* _ = fold_state file ~f:instantly_declare_toplevel in
  let* d = many file ~f:lookup_nodeclare_toplevel in
  return d
;;

let lookup builtins file =
  let s, file = run_pass (lookup_file file) ~init:(initial_state builtins) in
  match s.errs with
  | [] -> Ok file
  | errs -> Error errs
;;
