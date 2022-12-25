(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Errors

module Interpret (M : MONADERROR) = struct
  open M

  let lookup_name name (env : environment) = !(IdMap.find name env)

  let compare_values v v' =
    match v, v' with
    | VBool b, VBool b' -> return (compare_bool b b')
    | VInt n, VInt n' -> return (compare_int n n')
    | _ -> fail (RuntimeError "could not compare values")
  ;;

  let rec eval e (env : environment) =
    match e with
    | Const c -> eval_const c
    | Var name -> return (lookup_name name env)
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

  (* TODO: decide how to match argument labels *)
  and eval_app fu label arg env =
    eval fu env
    >>= fun closure_val ->
    match closure_val with
    | VClosure (env_of_fu, ArgNoLabel, None, name, fu_body) ->
      eval arg env
      >>= fun arg_val -> eval fu_body (IdMap.add name (ref arg_val) env_of_fu)
    | VClosure (env_of_fu, ArgLabeled l, None, name, fu_body) ->
      eval arg env >>= fun arg_val -> eval fu_body (IdMap.add l (ref arg_val) env_of_fu)
    | VClosure (env_of_fu, ArgOptional l, None, name, fu_body) ->
      eval arg env >>= fun arg_val -> eval fu_body (IdMap.add l (ref arg_val) env_of_fu)
    | VClosure (env_of_fu, ArgOptional l, Some e, name, fu_body) ->
      eval arg env >>= fun arg_val -> eval fu_body (IdMap.add l (ref arg_val) env_of_fu)
    | _ -> fail (RuntimeError "this is not a function")

  and eval_let name body exp env =
    eval body env >>= fun body_val -> eval exp (IdMap.add name (ref body_val) env)

  and eval_letrec name body exp env =
    let new_env = IdMap.add name (ref VUndef) env in
    eval body new_env
    >>= fun body_val ->
    match IdMap.find name new_env with
    | exception Not_found -> fail (RuntimeError "Couldn't update environment")
    | _ ->
      let new_env = IdMap.add name (ref body_val) new_env in
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