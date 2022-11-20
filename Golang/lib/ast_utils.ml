(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

open Ast

let rec show_typ = function
  | IntTyp -> "int"
  | StrTyp -> "string"
  | BoolTyp -> "bool"
  | ArrayTyp (len, eltyp) -> Printf.sprintf "[%d]%s" len (show_typ eltyp)
  | FunTyp { args; ret } ->
    let args = List.map (fun (_, typ) -> show_typ typ) args in
    let ret =
      match ret with
      | Void -> ""
      | One t -> " " ^ show_typ t
    in
    Printf.sprintf "fn(%s)%s" (String.concat ", " args) ret
;;

let eq_typ t1 t2 = compare_typ t1 t2 = 0
