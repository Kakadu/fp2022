(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

let rec show_typ = function
  | IntTyp -> "int"
  | StrTyp -> "string"
  | BoolTyp -> "bool"
  | ArrayTyp { el = eltyp } -> Printf.sprintf "[]%s" (show_typ eltyp)
  | FunTyp { args; ret } ->
    let args = List.map (fun (_, typ) -> show_typ typ) args in
    let ret =
      match ret with
      | Void -> ""
      | One t -> " " ^ show_typ t
    in
    Printf.sprintf "func(%s)%s" (String.concat ", " args) ret
  | ChanTyp t -> Printf.sprintf "chan %s" (show_typ t)
;;

let eq_typ t1 t2 = compare_typ t1 t2 = 0
