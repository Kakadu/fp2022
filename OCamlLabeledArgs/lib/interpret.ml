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
    match IdMap.find name env with
    | v -> v
    | exception Not_found -> VUndef
  ;;

  let upd_env name value (env : environment) : environment = IdMap.add name value env

  let rec eval e (env : environment) =
    match e with
    | Const c -> eval_const c
    | Var name -> return (find name env)
    | Binop _ -> eval_binop e env
    | Fun (label, default, name, exp) ->
      return (VClosure (None, env, Fun (label, default, name, exp)))
    | App (fu, label, arg) -> eval_app fu label arg env
    | IfThenElse (cond, tbody, fbody) -> eval_if cond tbody fbody env
    | Let (name, body, exp) -> eval_let name body exp env
    | LetRec (name, body, exp) -> eval_letrec name body exp env

  and eval_const = function
    | Bool b -> return (VBool b)
    | Int n -> return (VInt n)
    | Unit -> return VUnit

  and eval_if cond tbody fbody env =
    eval cond env
    >>= function
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
    >>= fun closure ->
    match closure with
    | VClosure (name, fu_env, fun_exp) ->
      eval arg env
      >>= fun arg_val ->
      let fun_unpack =
        match fun_exp with
        | Fun (lab, default, arg_name, fu_body) -> return (lab, default, arg_name, fu_body)
        | _ -> fail (RuntimeError "Not a function")
      in
      fun_unpack
      >>= fun data ->
      let lab, default, arg_name, fu_body = data in
      let default_value =
        match default with
        | Some e -> eval e env
        | None -> return VUndef
      in
      default_value
      >>= fun dv ->
      let has_unspecified_args, env_updated =
        match lab with
        | ArgNoLabel -> false, upd_env arg_name arg_val fu_env
        | ArgLabeled l -> false, upd_env l arg_val (upd_env arg_name arg_val fu_env)
        | ArgOptional l ->
          (match label with
           | ArgLabeled apply_l when compare_string apply_l l = 0 ->
             false, upd_env l arg_val (upd_env arg_name arg_val fu_env)
           | _ -> true, upd_env l dv (upd_env arg_name dv fu_env))
      in
      if has_unspecified_args
      then eval_app fu_body label arg env_updated
      else (
        match name with
        | Some n -> eval fu_body (upd_env n closure env_updated)
        | None -> eval fu_body env_updated)
    | _ -> fail (RuntimeError "This is not a function. It can not be applied.")

  and eval_let name body exp env =
    eval body env >>= fun body_val -> eval exp (upd_env name body_val env)

  and eval_letrec name body exp env =
    let env_updated = upd_env name (VClosure (Some name, env, body)) env in
    eval exp env_updated
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
