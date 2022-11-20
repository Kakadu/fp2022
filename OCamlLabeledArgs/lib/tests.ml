(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* ------------------------------------------------------ *)
(* -------------------- Parser tests -------------------- *)
(* ------------------------------------------------------ *)

open Parser
open Ast

let%test _ = parse identifier "_" = Ok "_"
let%test _ = parse ignored "\t\n\r " = Ok ()
let%test _ = parse integer "19" = Ok (Int 19)
let%test _ = parse expr_parser "name" = Ok (Var "name")
let%test _ = parse expr_parser "name1" = Ok (Var "name1")
let%test _ = parse expr_parser "_name" = Ok (Var "_name")
let%test _ = parse expr_parser "true" = Ok (Const (Bool true))
let%test _ = parse expr_parser "false" = Ok (Const (Bool false))
let%test _ = parse expr_parser "19" = Ok (Const (Int 19))
let%test _ = parse expr_parser "+19" = Ok (Const (Int 19))
let%test _ = parse expr_parser "-19" = Ok (Const (Int ~-19))
let%test _ = parse expr_parser "()" = Ok (Const Unit)
let%test _ = parse expr_parser "(  )" = Ok (Const Unit)
let%test _ = parse expr_parser "19" = Ok (Const (Int 19))
let%test _ = parse expr_parser "name" = Ok (Var "name")
let%test _ = parse expr_parser "name + name" = Ok (Binop (Plus, Var "name", Var "name"))
let%test _ = parse expr_parser "1 + 9" = Ok (Binop (Plus, Const (Int 1), Const (Int 9)))
let%test _ = parse expr_parser "fun x -> e" = Ok (Fun ("x", Var "e"))

let%test _ =
  parse expr_parser "1+(2+7)"
  = Ok (Binop (Plus, Const (Int 1), Binop (Plus, Const (Int 2), Const (Int 7))))
;;

let%test _ =
  parse expr_parser "if e1 then e2" = Ok (IfThenElse (Var "e1", Var "e2", Const Unit))
;;

let%test _ = parse expr_parser "let x = e in e'" = Ok (Let ("x", Var "e", Var "e'"))

let%test _ =
  parse expr_parser "let rec x = e in e'" = Ok (LetRec ("x", Var "e", Var "e'"))
;;

let%test _ =
  parse expr_parser "if e1 then e2 else e3"
  = Ok (IfThenElse (Var "e1", Var "e2", Var "e3"))
;;
