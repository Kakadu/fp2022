(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Ast

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
    (* TODO: decide how to pprint labels *)
    | Fun (label, default, name, e) -> fprintf ppf "fun %s -> %a" name printer e
    | App (fu, label, arg) -> fprintf ppf "(%a) (%a)" printer fu printer arg
    | Binop (op, l, r) -> fprintf ppf "(%s)" (op_to_str op)
  and pp_const ppf = function
    | Bool b -> pp_print_bool ppf b
    | Int n -> fprintf ppf "%d" n
    | Unit -> fprintf ppf "()"
  in
  printer
;;

let pp_typ =
  let rec printer ppf = function
    | TBool -> fprintf ppf "bool"
    | TInt -> fprintf ppf "int"
    | TVar x -> fprintf ppf "'_%s" x
    | TUnit -> fprintf ppf "unit"
    | Arrow ((Arrow (_l, _r) as l), r) -> fprintf ppf "(%a) -> %a" printer l printer r
    | Arrow (l, r) -> fprintf ppf "%a -> %a" printer l printer r
  in
  printer
;;

let pp_value =
  let rec printer ppf = function
    | VUndef -> fprintf ppf "undefined"
    | VBool b -> pp_print_bool ppf b
    | VInt n -> fprintf ppf "%d" n
    | VUnit -> fprintf ppf "()"
    | VClosure _ -> fprintf ppf "<fun>"
  in
  printer
;;