(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Utils

let binop_typefail (op : string) (l : value) (r : value) =
  typefail
    (String.concat
       ""
       [ "No candidates for "
       ; op
       ; " with arguments: "
       ; string_of_value l
       ; " and "
       ; string_of_value r
       ])
;;

let plus x y =
  match x, y with
  | Integer x, Integer y -> Integer (x + y)
  | String x, String y -> String (String.cat x y)
  | Array x, Array y -> Array (x @ y)
  | _ -> binop_typefail "+" x y
;;

let minus x y =
  match x, y with
  | Integer x, Integer y -> Integer (x - y)
  | _ -> binop_typefail "-" x y
;;

let multiply x y =
  match x, y with
  | Integer x, Integer y -> Integer (x * y)
  | String x, Integer y -> String (List.init y (fun _ -> x) |> String.concat "")
  | Array x, Integer y -> Array (List.init y (fun _ -> x) |> List.concat)
  | _ -> binop_typefail "*" x y
;;

let divide x y =
  match x, y with
  | Integer x, Integer y -> Integer (x / y)
  | _ -> binop_typefail "/" x y
;;

let raw_eq x y =
  match x, y with
  | Integer x, Integer y -> Some (x = y)
  | Bool x, Bool y -> Some (x = y)
  | String x, String y -> Some (String.equal x y)
  | Array x, Array y -> Some (x = y)
  | Nil, Nil -> Some true
  | _ -> None
;;

let eq x y =
  match raw_eq x y with
  | Some v -> Bool v
  | None -> binop_typefail "==" x y
;;

let neq x y =
  match raw_eq x y with
  | Some v -> Bool (not v)
  | None -> binop_typefail "!=" x y
;;

let and_op x y =
  match x, y with
  | Bool x, Bool y -> Bool (x && y)
  | _ -> binop_typefail "&&" x y
;;

let or_op x y =
  match x, y with
  | Bool x, Bool y -> Bool (x || y)
  | _ -> binop_typefail "||" x y
;;

let raw_gr x y =
  match x, y with
  | Integer x, Integer y -> Some (x > y)
  | String x, String y -> Some (String.compare x y > 0)
  | _ -> None
;;

let gr x y =
  match raw_gr x y with
  | Some v -> Bool v
  | _ -> binop_typefail ">" x y
;;

let gr_eq x y =
  match raw_gr x y, raw_eq x y with
  | Some v1, Some v2 -> Bool (v1 || v2)
  | _ -> binop_typefail ">=" x y
;;

let ls_eq x y =
  match raw_gr x y with
  | Some v -> Bool (not v)
  | _ -> binop_typefail "<=" x y
;;

let ls x y =
  match raw_gr x y, raw_eq x y with
  | Some v1, Some v2 -> Bool (not (v1 || v2))
  | _ -> binop_typefail "<" x y
;;

let match_binop = function
  | "+" -> plus
  | "-" -> minus
  | "*" -> multiply
  | "/" -> divide
  | "==" -> eq
  | "!=" -> neq
  | "&&" -> and_op
  | "||" -> or_op
  | ">" -> gr
  | ">=" -> gr_eq
  | "<=" -> ls_eq
  | "<" -> ls
  | op -> failwith ("Unknown binop " ^ op)
;;

let conditional (c : value) (t : ast) (e : ast) =
  match c with
  | Bool c -> if c then t else e
  | _ -> typefail "Conditional expects bool as condition"
;;

let indexing (v : value) (ind : value) =
  match v, ind with
  | Array v, Integer i -> List.nth v i
  | String v, Integer i -> String (String.get v i |> String.make 1)
  | _ -> binop_typefail "index" v ind
;;
