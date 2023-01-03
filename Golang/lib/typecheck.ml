(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Ast_utils
open Ident
open Pass

type typecheck_state =
  { file : ident source_file
  ; errs : string list
  ; ret : return_typ option
  }

module P = Pass (struct
  type t = typecheck_state
end)

open P

let add_err err =
  let* s = access in
  put { s with errs = err :: s.errs }
;;

let finish_with_err err =
  let* _ = add_err err in
  return None
;;

let set_ret ret =
  let* s = access in
  let old_ret = s.ret in
  let* _ = put { s with ret = Some ret } in
  return old_ret
;;

let enter_func ({ ret; _ } : 'a signature) = set_ret ret

let leave_func = function
  | Some ret -> set_ret ret *> return ()
  | None -> return ()
;;

let require_typ expected actual =
  if eq_typ expected actual
  then return ()
  else
    add_err
      (Printf.sprintf
         "Expected type %s but received %s"
         (show_typ expected)
         (show_typ actual))
;;

let require_typ_option expected = function
  | Some t -> require_typ expected t
  | None -> return ()
;;

type expr_value_used =
  | ValueUsed
  | Ignored

let rec resolve_typ id =
  let* { file; _ } = access in
  match Find_decl.find file id with
  | Some (Find_decl.Arg (_, t)) -> return (Some t)
  | Some (Find_decl.Var (_, e)) -> check_expr e
  | Some (Find_decl.Func (_, s, _)) ->
    return (Some (FunTyp (ident_sign_to_string_sign s)))
  | None -> return None

and require_expr_typ expected expr =
  let* t = check_expr expr in
  match t with
  | Some t -> require_typ expected t
  | None -> return ()

and check_exprs exprs = fold_state exprs ~f:(fun e -> check_expr e *> return ())

and check_expr = function
  | Const (Int _) -> return (Some IntTyp)
  | Const (Str _) -> return (Some StrTyp)
  | Const (Bool _) -> return (Some BoolTyp)
  | Ident id -> resolve_typ id
  | ArrLit (t, els) -> check_arr_lit t els
  | ArrIndex (arr, i) -> check_arr_index arr i
  | Call (f, args) -> check_call f args ~used:ValueUsed
  | FuncLit (sign, block) -> check_func_lit sign block
  | UnOp (op, expr) -> check_unop op expr
  | BinOp (l, op, r) -> check_binop l op r
  | Print _ -> finish_with_err "Cannot use print built-in's return value"
  | Len e -> check_len e
  | Append (arr, vs) -> check_append arr vs
  | Make t -> check_make t

and check_arr_lit array_typ els =
  let { el = el_typ } = array_typ in
  let* _ = fold_state els ~f:(require_expr_typ el_typ) in
  return (Some (ArrayTyp array_typ))

and check_arr_index arr i =
  let* arr_typ = check_expr arr in
  let* _ = require_expr_typ IntTyp i in
  match arr_typ with
  | Some (ArrayTyp { el = el_typ }) -> return (Some el_typ)
  | Some _ -> finish_with_err "Can only index arrays"
  | None -> return None

and check_call_args formal_args args =
  let formal_arg_types = List.map formal_args ~f:(fun (_, t) -> t) in
  match List.zip formal_arg_types args with
  | List.Or_unequal_lengths.Ok lst ->
    fold_state lst ~f:(fun (t, arg) -> require_expr_typ t arg)
  | List.Or_unequal_lengths.Unequal_lengths ->
    add_err
      (Printf.sprintf
         "Expected %d arguments but received %d"
         (List.length formal_arg_types)
         (List.length args))

and check_call_ret ret ~used =
  match ret, used with
  | Void, ValueUsed -> finish_with_err "Cannot use void as a value"
  | Void, Ignored -> return None
  | One ret, _ -> return (Some ret)

and check_call f args ~used =
  let* f_typ = check_expr f in
  match f_typ with
  | Some (FunTyp { args = formal_args; ret }) ->
    let* _ = check_call_args formal_args args in
    check_call_ret ret ~used
  | Some _ -> finish_with_err "Can only call functions"
  | None -> return None

and check_func_lit sign block =
  let* enclosing = enter_func sign in
  let* _ = check_block block in
  let* _ = leave_func enclosing in
  return (Some (FunTyp (ident_sign_to_string_sign sign)))

and check_unop op e =
  match op with
  | Minus -> require_expr_typ IntTyp e *> return (Some IntTyp)
  | Not -> require_expr_typ BoolTyp e *> return (Some BoolTyp)
  | Receive ->
    let* t = check_expr e in
    (match t with
     | Some (ChanTyp el) -> return (Some el)
     | Some _ -> finish_with_err "Can only receive (<-) from a channel"
     | None -> return None)

and check_binop l op r =
  let not_overloaded_op ~op ~ret =
    let* _ = require_expr_typ op l in
    let* _ = require_expr_typ op r in
    return (Some ret)
  in
  match op with
  | Mul | Div | Mod | Sub -> not_overloaded_op ~op:IntTyp ~ret:IntTyp
  | Lt | Lte | Gt | Gte -> not_overloaded_op ~op:IntTyp ~ret:BoolTyp
  | And | Or -> not_overloaded_op ~op:BoolTyp ~ret:BoolTyp
  | Eq | Neq ->
    let* _ = check_expr l in
    let* _ = check_expr r in
    return (Some BoolTyp)
  | Add ->
    let* lt = check_expr l in
    let* rt = check_expr r in
    (match lt, rt with
     | Some IntTyp, Some IntTyp -> return (Some IntTyp)
     | Some StrTyp, Some StrTyp -> return (Some StrTyp)
     | Some _, Some _ ->
       finish_with_err "Operator + is defined only for (int, int) and (string, string)"
     | _ -> return None)

and check_len e =
  let* t = check_expr e in
  match t with
  | Some (ArrayTyp _) -> return (Some IntTyp)
  | Some _ -> finish_with_err "len() built-in accepts only arrays"
  | None -> return None

and check_append arr vs =
  let* t = check_expr arr in
  match t with
  | None -> return None
  | Some (ArrayTyp { el }) ->
    let* _ = fold_state vs ~f:(require_expr_typ el) in
    return (Some (ArrayTyp { el }))
  | Some _ -> finish_with_err "First argument of append() must be an array"

and check_make = function
  | FunTyp t ->
    finish_with_err
      (Printf.sprintf "Type '%s' does not have a default value" (show_typ (FunTyp t)))
  | t -> return (Some t)

and check_block b = fold_state b ~f:check_stmt

and check_stmt = function
  | BlockStmt b -> check_block b
  | ExprStmt (Print args) -> check_exprs args
  | GoStmt (Call (f, args)) | ExprStmt (Call (f, args)) ->
    check_call f args ~used:Ignored *> return ()
  | ExprStmt expr | VarDecl (_, expr) -> check_expr expr *> return ()
  | AssignStmt (l, r) -> check_assign l r
  | RetStmt expr -> check_ret expr
  | IfStmt (cond, b1, b2) ->
    require_expr_typ BoolTyp cond *> check_block b1 *> check_block b2
  | ForStmt (cond, b) -> require_expr_typ BoolTyp cond *> check_block b
  | GoStmt _ -> add_err "go statement must be followed by a function call"
  | SendStmt (chan, x) -> check_send chan x

and check_assign l r =
  match l with
  | Ident _ | ArrIndex _ ->
    let* lt = check_expr l in
    (match lt with
     | Some lt -> require_expr_typ lt r
     | None -> return ())
  | _ -> add_err "Can only assign to variables and array elements"

and check_send chan x =
  let* chant = check_expr chan in
  match chant with
  | Some (ChanTyp eltyp) -> require_expr_typ eltyp x
  | Some _ -> add_err "Can only send (<-) into a channel"
  | None -> return ()

and check_ret e =
  let* s = access in
  match s.ret, e with
  | None, _ -> add_err "Return statement outside of function"
  | Some (One t), Some e -> require_expr_typ t e
  | Some (One t), None ->
    add_err (Printf.sprintf "Function must return %s but returns void" (show_typ t))
  | Some Void, Some _ ->
    add_err (Printf.sprintf "Function must return void but returns something else")
  | Some Void, None -> return ()
;;

let check_global_var e =
  match e with
  | Const _ -> check_expr e *> return ()
  | _ -> add_err "At top level global variables can only have constant initializers"
;;

let check_func_decl name s b =
  let check_main_sign { args; ret } =
    match args, ret with
    | [], Void -> return ()
    | _, _ -> add_err "main() function must accept zero arguments and return nothing"
  in
  let* _ =
    match Ident.name name with
    | "main" -> check_main_sign s
    | _ -> return ()
  in
  check_func_lit s b *> return ()
;;

let check_toplevel = function
  | GlobalVarDecl (_, e) -> check_global_var e
  | FuncDecl (n, s, b) -> check_func_decl n s b
;;

let check_file file = fold_state file ~f:check_toplevel

let check file =
  let s, _ = run_pass (check_file file) ~init:{ file; ret = None; errs = [] } in
  match s.errs with
  | [] -> Ok ()
  | errs -> Error errs
;;
