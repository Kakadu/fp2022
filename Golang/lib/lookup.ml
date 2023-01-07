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

  let rec get_global scope =
    match scope.parent with
    | Some p -> get_global p
    | None -> scope
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

let initial_state =
  let scope = Scope.empty_global_scope [] in
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

let is_defined name =
  let* s = access in
  match Scope.resolve s.scope name with
  | Some _ -> return true
  | None -> return false
;;

let enter_scope : Scope.t t =
  let* state = access in
  let id = state.next_id in
  let old_scope = state.scope in
  let scope = Scope.empty_local_scope old_scope id in
  let* _ = put { state with next_id = id + 1; scope } in
  return old_scope
;;

let get_scope =
  let* state = access in
  return state.scope
;;

let set_scope scope : unit t =
  let* state = access in
  put { state with scope }
;;

let enter_fn_scope = enter_scope

(* Expressions *)

let err_expr = Ident Ident.error_ident

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
  | Call (f, args) -> lookup_func_call f args
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
  | Append (arr, vs) ->
    let* arr = lookup_expr arr in
    let* vs = lookup_exprs vs in
    return (Append (arr, vs))
  | Make x -> return (Make x)

and lookup_builtin name args =
  match name with
  | "print" -> return (Print args)
  | "append" ->
    (match args with
     | arr :: first :: rest -> return (Append (arr, first :: rest))
     | _ ->
       add_err "append() takes at least 2 arguments" *> return (Append (err_expr, [])))
  | "len" ->
    (match args with
     | [ x ] -> return (Len x)
     | _ -> add_err "len() built-in takes exactly 1 argument" *> return (Len err_expr))
  | name -> add_err ("Identifier " ^ name ^ " is not declared!") *> return err_expr

and lookup_func_call f args =
  let* args = lookup_exprs args in
  let as_user_func =
    let* f = lookup_expr f in
    return (Call (f, args))
  in
  match f with
  | Ident name ->
    let* is_def = is_defined name in
    if is_def then as_user_func else lookup_builtin name args
  | _ -> as_user_func

and lookup_func sign b =
  (* Closures are not allowed here *)
  let* s = enter_fn_scope in
  let* sign = lookup_declare_signature sign in
  let* b = lookup_block b in
  let* _ = set_scope s in
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
  | SendStmt (chan, x) ->
    let* chan = lookup_expr chan in
    let* x = lookup_expr x in
    return (SendStmt (chan, x))

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

let lookup file =
  let s, file = run_pass (lookup_file file) ~init:initial_state in
  match s.errs with
  | [] -> Ok file
  | errs -> Error errs
;;
