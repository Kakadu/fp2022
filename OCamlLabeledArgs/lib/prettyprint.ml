(** Copyright 2022-2023, Denis Porsev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Parsetree
open Typedtree
open Errors

let pp_expr =
  let op_to_str = function
    | Plus -> "+"
    | Minus -> "-"
    | Mult -> "*"
    | Divide -> "/"
    | Mod -> "mod"
    | Eq -> "="
    | Neq -> "<>"
    | Lt -> "<"
    | Ltq -> "<="
    | Gt -> ">"
    | Gtq -> ">="
  in
  let rec printer ppf = function
    | Const c -> pp_const ppf c
    | IfThenElse (cond, tbody, fbody) ->
      fprintf ppf "if %a then %a else %a" printer cond printer tbody printer fbody
    | Var x -> pp_print_string ppf x
    | Let (name, body, in_e) ->
      fprintf ppf "let %s = %a in %a" name printer body printer in_e
    | LetRec (name, body, in_e) ->
      fprintf ppf "let rec %s = %a in %a" name printer body printer in_e
    | Fun (label, default, name, e) -> pp_fun ppf label default name e
    | App (fu, label, arg) -> pp_app ppf fu label arg
    | Binop (op, l, r) -> fprintf ppf "(%a %s %a)" printer l (op_to_str op) printer r
  and pp_const ppf = function
    | Bool b -> pp_print_bool ppf b
    | Int n -> fprintf ppf "%d" n
    | Unit -> fprintf ppf "()"
  and pp_fun ppf label default name e =
    match label with
    | ArgNoLabel -> fprintf ppf "fun %s -> %a" name printer e
    | ArgLabeled s -> fprintf ppf "fun ~%s:%s -> %a" s name printer e
    | ArgOptional s ->
      (match default with
       | None -> fprintf ppf "fun ?%s -> %a" s printer e
       | Some def_e ->
         fprintf ppf "fun ?%s:(%s = %a) -> %a" s name printer def_e printer e)
  and pp_app ppf fu label arg =
    match label with
    | ArgNoLabel -> fprintf ppf "(%a %a)" printer fu printer arg
    | ArgLabeled s | ArgOptional s -> fprintf ppf "(%a ~%s:%a)" printer fu s printer arg
  in
  printer
;;

let pp_typ =
  let rec printer ppf = function
    | TBool -> fprintf ppf "bool"
    | TInt -> fprintf ppf "int"
    | TVar x -> fprintf ppf "'%s" x
    | TUnit -> fprintf ppf "unit"
    | Arrow (l, label, r) ->
      (match label with
       | ArgNoLabel -> fprintf ppf "%a -> %a" printer l printer r
       | ArgLabeled lab -> fprintf ppf "~%s:%a -> %a" lab printer l printer r
       | ArgOptional lab -> fprintf ppf "?%s:%a -> %a" lab printer l printer r)
  in
  printer
;;

let pp_value =
  let printer ppf = function
    | VUndef -> fprintf ppf "undefined"
    | VBool b -> pp_print_bool ppf b
    | VInt n -> fprintf ppf "%d" n
    | VUnit -> fprintf ppf "()"
    | VClosure _ -> fprintf ppf "<fun>"
  in
  printer
;;

let pp_error =
  let printer ppf = function
    | ParseError s -> fprintf ppf "Parser error: %s\n" s
    | TypeError s ->
      fprintf ppf "Type error: %s\n" s (* TODO: better type error handling *)
    | RuntimeError s -> fprintf ppf "Runtime error: %s\n" s
  in
  printer
;;

let pp_env ppf (env : environment) =
  IdMap.iter (fun name value -> Format.fprintf ppf "%s : %a\n" name pp_value !value) env
;;
