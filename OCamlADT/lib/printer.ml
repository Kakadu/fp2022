(** Copyright 2021-2022, Kakadu, EmirVildanov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Pretty printer module *)

open Ast
open Typing

exception PrinterException of string

let rec type_to_string = function
  | BaseT b ->
    (match b with
     | Int -> "int"
     | String -> "string"
     | Bool -> "bool"
     | Nil -> "[]"
     | Unit -> "()")
  | TypeVariable id -> Printf.sprintf "%s%s" "'" (string_of_int id)
  | NamedT (name, t) ->
    (match t with
     | None -> name
     | Some t -> Printf.sprintf "%s %s" (type_to_string t) name)
  | AdtT _ -> "ADT"
  | ArrowT (left, right) ->
    Printf.sprintf "%s -> %s" (type_to_string left) (type_to_string right)
  | TupleT ts ->
    List.fold_left (fun acc x -> Printf.sprintf "%s * %s" acc (type_to_string x)) "" ts
  | ListT t -> Printf.sprintf "%s %s" (type_to_string t) " list"
;;

let binop_to_string = function
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Divide -> "/"
  | Eq -> "="
;;

let unop_to_string = function
  | UnaryMinus -> "- "
;;

let rec expr_to_string = function
  | Constant x -> const_to_string x
  | Cons _ as l -> string_of_list l
  | IfThenElse (b, e, e') ->
    let sb, se, se' = expr_to_string b, expr_to_string e, expr_to_string e' in
    Printf.sprintf "if %s then %s else %s" sb se se'
  | Let (flag, id, _, expr) ->
    let se = expr_to_string expr in
    if flag
    then Printf.sprintf "let rec %s = %s" id se
    else Printf.sprintf "let %s = %s" id se
  | BinaryOp (op, l, r) ->
    let sl, sop, sr = expr_to_string l, binop_to_string op, expr_to_string r in
    Printf.sprintf "(%s %s %s)" sl sop sr
  | UnaryOp (op, e) -> Printf.sprintf "%s%s" (unop_to_string op) (expr_to_string e)
  | Fun (x, e) -> Printf.sprintf "fun %s -> %s" x (expr_to_string e)
  | App (e, e') -> Printf.sprintf "%s (%s)" (expr_to_string e) (expr_to_string e')
  | Var x -> x
  | Match (e, pes) ->
    let se = expr_to_string e in
    let sps =
      List.fold_right
        (fun (p, e) a ->
          Printf.sprintf "| %s -> %s %s" (expr_to_string p) (expr_to_string e) a)
        pes
        ""
    in
    Printf.sprintf "match %s with %s" se sps
  | Tuple exprs ->
    Printf.sprintf
      "(%s)"
      (List.fold_left
         (fun acc expr -> Printf.sprintf "%s, %s" acc (expr_to_string expr))
         ""
         exprs)
  | ADT (name, exprs) ->
    Printf.sprintf
      "%s (%s)"
      name
      (Printf.sprintf
         "(%s)"
         (List.fold_left
            (fun acc expr -> Printf.sprintf "%s, %s" acc (expr_to_string expr))
            ""
            exprs))
  | Type (name, t) -> Printf.sprintf "type %s = %s" name (type_to_string t)
  | LetIn (let_expr, in_expr) ->
    Printf.sprintf "%s in %s" (expr_to_string let_expr) (expr_to_string in_expr)

and string_of_list l =
  let rec string_of_list' = function
    | Cons (h, Constant Nil) -> expr_to_string h
    | Cons (h, t) -> Printf.sprintf "%s; %s" (expr_to_string h) (string_of_list' t)
    | _ ->
      raise (PrinterException "string_of_list should only be used on non-empty lists")
  in
  Printf.sprintf "[%s]" (string_of_list' l)

and const_to_string = function
  | Bool b -> string_of_bool b
  | Str s -> s
  | Int n -> string_of_int n
  | Unit -> "()"
  | Nil -> "[]"
;;

let rec val_to_string = function
  | VUndef -> "undefined"
  | VBool b -> string_of_bool b
  | VInt i -> string_of_int i
  | VString s -> s
  | VNil -> "[]"
  | VUnit -> "()"
  | VCons (v1, v2) -> Printf.sprintf "[%s]" (list_to_string v1 v2)
  | VClosure (_, _, _) -> "<fun>"
  | VRecClosure (_, _, _, _) -> "<rec fun>"

and list_to_string v1 = function
  | VNil -> val_to_string v1
  | VCons (v1', v2') ->
    Printf.sprintf "%s; %s" (val_to_string v1) (list_to_string v1' v2')
  | _ -> raise (PrinterException "list_to_string: Error in typechecker.")
;;
