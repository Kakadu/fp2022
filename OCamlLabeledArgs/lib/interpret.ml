(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base

let lookup_name name env = !(IdMap.find name env)

let compare_values v v' =
  match v, v' with
  | VBool b, VBool b' -> compare_bool b b'
  | VInt n, VInt n' -> compare_int n n'
  | _ -> failwith "Runtime failure: error in comparison"
;;

let rec eval e (env : environment) : value =
  match e with
  | Const c -> eval_const c
  | Var name -> lookup_name name env
  | Binop _ -> eval_binop e env
  | Fun (label, default, name, exp) -> VClosure (env, label, default, name, exp)
  | App (fu, label, arg) -> eval_app fu label arg env
  | IfThenElse (cond, tbody, fbody) -> eval_if cond tbody fbody env
  | Let (name, body, exp) -> eval_let name body exp env
  | LetRec (name, body, exp) -> eval_letrec name body exp env

and eval_const c =
  match c with
  | Bool b -> VBool b
  | Int n -> VInt n
  | Unit -> VUnit

and eval_if cond tbody fbody env =
  match eval cond env with
  | VBool b -> eval (if b then tbody else fbody) env
  | _ -> failwith "Runtime failure: type error in if condition"

and eval_binop e env =
  match e with
  | Binop (op, l, r) ->
    (match op with
     | Plus -> eval_arithm ( + ) l r env
     | Minus -> eval_arithm ( - ) l r env
     | Mult -> eval_arithm ( * ) l r env
     | Divide -> eval_arithm ( / ) l r env
     | Mod -> eval_arithm ( mod ) l r env
     | Eq -> eval_cmp ( = ) l r env
     | Neq -> eval_cmp ( <> ) l r env
     | Lt -> eval_cmp ( < ) l r env
     | Ltq -> eval_cmp ( <= ) l r env
     | Gt -> eval_cmp ( > ) l r env
     | Gtq -> eval_cmp ( >= ) l r env)
  | _ -> failwith "Runtime failure: error in binary operation"

and eval_arithm op l r env =
  let le = eval l env in
  let re = eval r env in
  match le, re with
  | VInt n, VInt n' -> VInt (op n n')
  | VInt _, _ -> failwith "Runtime failure: type error with right operand"
  | _ -> failwith "Runtime failure: type error with left operand"

and eval_cmp op l r env =
  let le = eval l env in
  let re = eval r env in
  VBool (op (compare_values le re) 0)

(* TODO: decide how to match argument labels *)
and eval_app fu label arg env =
  match eval fu env with
  | VClosure (env_of_fu, ArgNoLabel, None, name, fu_body) ->
    let arg_value = eval arg env in
    eval fu_body (IdMap.add name (ref arg_value) env_of_fu)
  | VClosure (env_of_fu, ArgLabeled l, None, name, fu_body) ->
    let arg_value = eval arg env in
    eval fu_body (IdMap.add name (ref arg_value) env_of_fu)
  | VClosure (env_of_fu, ArgOptional l, Some e, name, fu_body) ->
    let arg_value = eval arg env in
    eval fu_body (IdMap.add name (ref arg_value) env_of_fu)
  | _ -> failwith "Runtime failure: this is not a function"

and eval_let name body exp env =
  let body_value = eval body env in
  eval exp (IdMap.add name (ref body_value) env)

and eval_letrec name body exp env =
  let new_env = IdMap.add name (ref VUndef) env in
  let body_value = eval body new_env in
  let upd_error =
    (* FIXME: zanuda linter will scream about this *)
    try IdMap.find name env := body_value with
    | Not_found -> failwith "Runtime error: Couldn't update environemt"
  in
  eval exp new_env
;;
