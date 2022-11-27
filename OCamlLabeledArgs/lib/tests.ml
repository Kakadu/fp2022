(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* ------------------------------------------------------ *)
(* -------------------- Parser tests -------------------- *)
(* ------------------------------------------------------ *)

open Parser
open Ast

(* Base combinators *)
let%test _ = parse identifier "_" = Ok "_"
let%test _ = parse ignored "\t\n\r " = Ok ()
(* Arguments combinator*)
let%test _ = parse arg_parser "label" = Ok (ArgNoLabel "label")
let%test _ = parse arg_parser "~label" = Ok (ArgLabelled "label")
let%test _ = parse arg_parser "?label" = Ok (ArgOptional "label")
(* Var combinator *)
let%test _ = parse expr_parser "name" = Ok (Var "name")
let%test _ = parse expr_parser "name1" = Ok (Var "name1")
let%test _ = parse expr_parser "_name" = Ok (Var "_name")
let%test _ = parse expr_parser "_name1'" = Ok (Var "_name1'")
(* Const combinator *)
let%test _ = parse expr_parser "true" = Ok (Const (Bool true))
let%test _ = parse expr_parser "false" = Ok (Const (Bool false))
let%test _ = parse expr_parser "19" = Ok (Const (Int 19))
let%test _ = parse expr_parser "+19" = Ok (Const (Int 19))
let%test _ = parse expr_parser "-19" = Ok (Const (Int ~-19))
let%test _ = parse expr_parser "()" = Ok (Const Unit)
let%test _ = parse expr_parser "(  )" = Ok (Const Unit)
(* Binop combinator *)
let%test _ = parse expr_parser "name - name" = Ok (Binop (Minus, Var "name", Var "name"))
let%test _ = parse expr_parser "1 + 9" = Ok (Binop (Plus, Const (Int 1), Const (Int 9)))

let%test _ =
  parse expr_parser "1 * (3 / 9)"
  = Ok (Binop (Mult, Const (Int 1), Binop (Divide, Const (Int 3), Const (Int 9))))
;;

let%test _ =
  parse expr_parser "(1 >= 3) < 9"
  = Ok (Binop (Lt, Binop (Gtq, Const (Int 1), Const (Int 3)), Const (Int 9)))
;;

(* Lambda combinator (fun x -> e) *)
let%test _ = parse expr_parser "fun x -> e" = Ok (Fun ("x", Var "e"))
let%test _ = parse expr_parser "fun x y -> e" = Ok (Fun ("x", Fun ("y", Var "e")))

(* If-then-else combinator (if b then e else e') *)
let%test _ =
  parse expr_parser "if e1 then e2" = Ok (IfThenElse (Var "e1", Var "e2", Const Unit))
;;

let%test _ =
  parse expr_parser "if e1 then e2 else e3"
  = Ok (IfThenElse (Var "e1", Var "e2", Var "e3"))
;;

(* Let combinator (let x = e in e') *)
let%test _ = parse expr_parser "let x = e in e'" = Ok (Let ("x", Var "e", Var "e'"))

let%test _ =
  parse expr_parser "let x = 1 in let y = 9 in x <= y"
  = Ok (Let ("x", Const (Int 1), Let ("y", Const (Int 9), Binop (Ltq, Var "x", Var "y"))))
;;

let%test _ =
  parse expr_parser "let rec x = e in e'" = Ok (LetRec ("x", Var "e", Var "e'"))
;;

(* Definition combinator (let f x = x) *)
let%test _ =
  parse definition_parser "let f x y z = x + y + z"
  = Ok
      ( "f"
      , Fun
          ( "x"
          , Fun ("y", Fun ("z", Binop (Plus, Binop (Plus, Var "x", Var "y"), Var "z"))) )
      )
;;

(* Combination of combinators *)

(* (1) Factorial *)
let%test _ =
  parse definition_parser "let rec fact n = if n = 0 then 1 else n * fact (n - 1)"
  = Ok
      ( "fact"
      , LetRec
          ( "fact"
          , Fun
              ( "n"
              , IfThenElse
                  ( Binop (Eq, Var "n", Const (Int 0))
                  , Const (Int 1)
                  , Binop
                      ( Mult
                      , Var "n"
                      , App (Var "fact", Binop (Minus, Var "n", Const (Int 1))) ) ) )
          , Var "fact" ) )
;;

(* (2) X into power of Y *)
let%test _ =
  parse definition_parser "let rec pow x y = if y = 0 then 1 else x * pow x (y - 1)"
  = Ok
      ( "pow"
      , LetRec
          ( "pow"
          , Fun
              ( "x"
              , Fun
                  ( "y"
                  , IfThenElse
                      ( Binop (Eq, Var "y", Const (Int 0))
                      , Const (Int 1)
                      , Binop
                          ( Mult
                          , Var "x"
                          , App
                              ( Var "pow"
                              , App (Var "x", Binop (Minus, Var "y", Const (Int 1))) ) )
                      ) ) )
          , Var "pow" ) )
;;

(* (3) Increment *)
let%test _ =
  parse definition_parser "let inc = fun x -> x + 1"
  = Ok ("inc", Fun ("x", Binop (Plus, Var "x", Const (Int 1))))
;;