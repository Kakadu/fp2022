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
let%test _ = parse label_parser "~label:" = Ok (ArgLabeled "label")
let%test _ = parse label_parser "?label:" = Ok (ArgOptional "label")
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
let%test _ = parse expr_parser "fun x -> e" = Ok (Fun (ArgNoLabel, None, "x", Var "e"))

let%test _ =
  parse expr_parser "fun x y -> e"
  = Ok (Fun (ArgNoLabel, None, "x", Fun (ArgNoLabel, None, "y", Var "e")))
;;

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
          ( ArgNoLabel
          , None
          , "x"
          , Fun
              ( ArgNoLabel
              , None
              , "y"
              , Fun
                  ( ArgNoLabel
                  , None
                  , "z"
                  , Binop (Plus, Binop (Plus, Var "x", Var "y"), Var "z") ) ) ) )
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
              ( ArgNoLabel
              , None
              , "n"
              , IfThenElse
                  ( Binop (Eq, Var "n", Const (Int 0))
                  , Const (Int 1)
                  , Binop
                      ( Mult
                      , Var "n"
                      , App (ArgNoLabel, Binop (Minus, Var "n", Const (Int 1)), Var "fact")
                      ) ) )
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
              ( ArgNoLabel
              , None
              , "x"
              , Fun
                  ( ArgNoLabel
                  , None
                  , "y"
                  , IfThenElse
                      ( Binop (Eq, Var "y", Const (Int 0))
                      , Const (Int 1)
                      , Binop
                          ( Mult
                          , Var "x"
                          , App
                              ( ArgNoLabel
                              , App
                                  ( ArgNoLabel
                                  , Binop (Minus, Var "y", Const (Int 1))
                                  , Var "x" )
                              , Var "pow" ) ) ) ) )
          , Var "pow" ) )
;;

(* (3) Increment *)
let%test _ =
  parse definition_parser "let inc = fun x -> x + 1"
  = Ok ("inc", Fun (ArgNoLabel, None, "x", Binop (Plus, Var "x", Const (Int 1))))
;;

(* (4) Labeled arguments *)
let%test _ =
  parse definition_parser "let f ~name1:x ~name2:y = x + y"
  = Ok
      ( "f"
      , Fun
          ( ArgLabeled "name1"
          , None
          , "x"
          , Fun (ArgLabeled "name2", None, "y", Binop (Plus, Var "x", Var "y")) ) )
;;

(* (5) Labeled arguments syntactic sugar *)
let%test _ =
  parse definition_parser "let f ~x ~y = x + y"
  = Ok
      ( "f"
      , Fun
          ( ArgLabeled "x"
          , None
          , ""
          , Fun (ArgLabeled "y", None, "", Binop (Plus, Var "x", Var "y")) ) )
;;

(* (6) Optional arguments *)
let%test _ =
  parse definition_parser "let f ~name1:x ?y:(y = 0) = x + y"
  = Ok
      ( "f"
      , Fun
          ( ArgLabeled "name1"
          , None
          , "x"
          , Fun
              (ArgOptional "y", Some (Const (Int 0)), "y", Binop (Plus, Var "x", Var "y"))
          ) )
;;

(* (7) Optional arguments without default value *)
let%test _ =
  parse definition_parser "let f ~name1:x ?y = x + y"
  = Ok
      ( "f"
      , Fun
          ( ArgLabeled "name1"
          , None
          , "x"
          , Fun (ArgOptional "y", None, "", Binop (Plus, Var "x", Var "y")) ) )
;;

(* (8) More optional arguments *)
let%test _ =
  parse definition_parser "let test ?x:(x = 0) ?y:(y = 0) () ?z:(z = 0) () = x + y + z"
  = Ok
      ( "test"
      , Fun
          ( ArgOptional "x"
          , Some (Const (Int 0))
          , "x"
          , Fun
              ( ArgOptional "y"
              , Some (Const (Int 0))
              , "y"
              , Fun
                  ( ArgNoLabel
                  , None
                  , ""
                  , Fun
                      ( ArgOptional "z"
                      , Some (Const (Int 0))
                      , "z"
                      , Fun
                          ( ArgNoLabel
                          , None
                          , ""
                          , Binop (Plus, Binop (Plus, Var "x", Var "y"), Var "z") ) ) ) )
          ) )
;;

(* (9) Labeled arguments in function call *)
let%test _ =
  parse expr_parser "f ~name2:8 ~name1:9"
  = Ok
      (App
         ( ArgLabeled "name2"
         , Const (Int 8)
         , App (ArgLabeled "name1", Const (Int 9), Var "f") ))
;;

(* The definition of the above *)
let%test _ =
  parse definition_parser "let f ~name2 ~name1 = name1 + name2"
  = Ok
      ( "f"
      , Fun
          ( ArgLabeled "name2"
          , None
          , ""
          , Fun (ArgLabeled "name1", None, "", Binop (Plus, Var "name1", Var "name2")) )
      )
;;