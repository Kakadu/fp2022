(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

open Base
open Ast
open Ident
open Pass

type vfunc = ident signature * ident block

type value =
  | VInt of int
  | VBool of bool
  | VStr of string
  | VArr of value list
  | VFunc of vfunc
  | VVoid

type eval_state =
  { tbl : value tbl
  ; returned : value option
  }

module P = Pass (struct
  type t = eval_state
end)

open P

let set id value =
  let* s = access in
  put { s with tbl = Ident.set s.tbl ~key:id ~data:value }
;;

let get id =
  let* s = access in
  match Ident.find s.tbl id with
  | Some value -> return value
  | None -> failwith "Value not found!"
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
  | Ident id -> get id
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
      fold_state lst ~f:(fun (farg, arg) -> set farg arg)
    | List.Or_unequal_lengths.Unequal_lengths ->
      failwith "Internal error: illegal number of args"
  in
  match f with
  | VFunc ({ args = fargs; _ }, b) ->
    let* _ = set_args fargs args in
    let* _ = eval_block b in
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
  | Ident id -> set id r
  | _ -> failwith "Illegal assignment"

and eval_vardecl (id, expr) =
  let* value = eval_expr expr in
  set id value

and eval_if cond bthen belse =
  let* cond = eval_expr cond in
  match cond with
  | VBool true -> eval_block bthen
  | VBool false -> eval_block belse
  | _ -> failwith "Internal error: illegal types"
;;

let eval_file file =
  let assign_toplevel = function
    | GlobalVarDecl v -> eval_vardecl v
    | FuncDecl (id, sign, b) ->
      let* v = eval_func_lit sign b in
      set id v
  in
  let eval_main = function
    | FuncDecl (id, _, b) when String.equal (name id) "main" -> eval_block b
    | _ -> return ()
  in
  let* _ = fold_state file ~f:assign_toplevel in
  fold_state file ~f:eval_main
;;

let eval file =
  let _ = run_pass (eval_file file) ~init:{ tbl = empty_tbl; returned = None } in
  ()
;;
