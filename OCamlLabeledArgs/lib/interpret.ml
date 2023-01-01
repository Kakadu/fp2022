(** Copyright 2022-2023, Denis Porsev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Parsetree
open Typedtree
open Errors

module Interpret (M : MONADERROR) = struct
  open M

  let compare_values v v' =
    match v, v' with
    | VBool b, VBool b' -> return (compare_bool b b')
    | VInt n, VInt n' -> return (compare_int n n')
    | _ -> fail (RuntimeError "could not compare values")
  ;;

  let find name (env : environment) =
    !(match IdMap.find name env with
      | v -> v
      | exception Not_found -> ref VUndef)
  ;;

  let upd_env name value (env : environment) : environment =
    IdMap.add name (ref value) env
  ;;

  let rec eval e (env : environment) =
    match e with
    | Const c -> eval_const c
    | Var name -> return (find name env)
    | Binop _ -> eval_binop e env
    | Fun (label, default, name, exp) ->
      return (VClosure (env, label, default, name, exp))
    | App (fu, label, arg) -> eval_app fu label arg env
    | IfThenElse (cond, tbody, fbody) -> eval_if cond tbody fbody env
    | Let (name, body, exp) -> eval_let name body exp env
    | LetRec (name, body, exp) -> eval_letrec name body exp env

  and eval_const c =
    match c with
    | Bool b -> return (VBool b)
    | Int n -> return (VInt n)
    | Unit -> return VUnit

  and eval_if cond tbody fbody env =
    eval cond env
    >>= fun cond_val ->
    match cond_val with
    | VBool b -> eval (if b then tbody else fbody) env
    | _ -> fail (RuntimeError "error in if condition")

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
    | _ -> fail (RuntimeError "error in binary operation")

  and eval_arithm op l r env =
    eval l env
    >>= fun l_val ->
    eval r env
    >>= fun r_val ->
    match l_val, r_val with
    | VInt n, VInt n' -> return (VInt (op n n'))
    | VInt _, _ -> fail (RuntimeError "type error with right operand")
    | _ -> fail (RuntimeError "type error with left operand")

  and eval_cmp op l r env =
    eval l env
    >>= fun l_val ->
    eval r env
    >>= fun r_val -> compare_values l_val r_val >>= fun c -> return (VBool (op c 0))

  and eval_app fu label arg env =
    eval fu env
    >>= fun closure_val ->
    match closure_val with
    | VClosure (fu_env, ArgNoLabel, None, name, fu_body) ->
      eval arg env >>= fun arg_val -> eval fu_body (upd_env name arg_val fu_env)
    | VClosure (fu_env, ArgLabeled l, None, name, fu_body) ->
      eval arg env
      >>= fun arg_val -> eval fu_body (upd_env l arg_val (upd_env name arg_val fu_env))
    | VClosure (fu_env, ArgOptional l, None, name, fu_body) ->
      eval arg env
      >>= fun arg_val -> eval fu_body (upd_env l arg_val (upd_env name arg_val fu_env))
    | VClosure (fu_env, ArgOptional l, Some e, name, fu_body) ->
      (match label with
       | ArgLabeled apply_l ->
         (if compare_string apply_l l = 0 then eval arg env else eval e env)
         >>= fun arg_val -> eval fu_body (upd_env l arg_val (upd_env name arg_val fu_env))
       | _ ->
         eval e env
         >>= fun arg_val ->
         eval_app fu_body label arg (upd_env l arg_val (upd_env name arg_val fu_env)))
    | _ -> fail (RuntimeError "This is not a function. It can not be applied.")

  and eval_let name body exp env =
    eval body env >>= fun body_val -> eval exp (upd_env name body_val env)

  and eval_letrec name body exp env =
    let new_env = upd_env name VUndef env in
    eval body new_env
    >>= fun body_val ->
    match IdMap.find name new_env with
    | exception Not_found -> fail (RuntimeError "Couldn't update environment")
    | v ->
      (* Use of assignment because there are mutable values in IdMap *)
      v := body_val;
      eval exp new_env
  ;;
end

module EvalResult : MONADERROR with type 'a t = ('a, error) result = struct
  type 'a t = ('a, error) result

  let ( >>= ) value func =
    match value with
    | Ok x -> func x
    | Error s -> Error s
  ;;

  let return x = Ok x
  let fail error = Error error
end