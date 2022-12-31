(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

(* ------------------------------------------------------ *)
(* --------------------- Test cases --------------------- *)
(* ------------------------------------------------------ *)

(* (1) Factorial *)
let test_factorial_definition = "let rec fact n = if n = 0 then 1 else n * fact (n - 1)"
let test_factorial_call = "fact 5"
let test_factorial_expression = test_factorial_definition ^ " in " ^ test_factorial_call

let test_factorial_expected =
  let rec fact n = if n = 0 then 1 else n * fact (n - 1) in
  fact 5
;;

(* (2) X into the power of Y *)
let test_x_power_y_definition =
  "let rec pow x y = if y = 0 then 1 else x * pow (x) (y - 1)"
;;

let test_x_power_y_call = "pow 4 5"
let test_x_power_y_expression = test_x_power_y_definition ^ " in " ^ test_x_power_y_call

let test_x_power_y_expected =
  let rec pow x y = if y = 0 then 1 else x * pow x (y - 1) in
  pow 4 5
;;

(* (3) Increment *)
let test_increment_definition = "let inc = fun x -> x + 1"
let test_increment_call = "inc 4"
let test_increment_expression = test_increment_definition ^ " in " ^ test_increment_call

let test_increment_expected =
  let inc x = x + 1 in
  inc 4
;;

(* (4) Labeled arguments *)
let test_labeled_arguments_definition = "let f ~name1:x ~name2:y = x / y"
let test_labeled_arguments_call = "f ~name1:4 ~name2:5"

let test_labeled_arguments_expression =
  test_labeled_arguments_definition ^ " in " ^ test_labeled_arguments_call
;;

let test_labeled_arguments_expected =
  let f ~name1:x ~name2:y = x / y in
  f ~name1:4 ~name2:5
;;

(* (5) Labeled arguments syntactic sugar *)
let test_labeled_arguments_sugar_definition = "let f ~x ~y = x + y"
let test_labeled_arguments_sugar_call = "f ~x:4 ~y:5"

let test_labeled_arguments_sugar_expression =
  test_labeled_arguments_sugar_definition ^ " in " ^ test_labeled_arguments_sugar_call
;;

let test_labeled_arguments_sugar_expected =
  let f ~x ~y = x + y in
  f ~x:4 ~y:5
;;

(* (6) Optional arguments *)
let test_optional_arguments_definition = "let f ?name:(arg1=4) arg2 = arg1 + arg2"
let test_optional_arguments_call = "f 5"

let test_optional_arguments_expression =
  test_optional_arguments_definition ^ " in " ^ test_optional_arguments_call
;;

let test_optional_arguments_expected =
  let f ?name:(arg1 = 4) arg2 = arg1 + arg2 in
  f 5
;;

(* (7) Complex optional arguments *)
let test_optional_arguments_complex_definition =
  "let f ?x:(x = 0) ?y:(y = 0) () ?z:(z = 0) () = x + y + z"
;;

let test_optional_arguments_complex_call = "f ~x:5"

let test_optional_arguments_complex_expression =
  test_optional_arguments_complex_definition
  ^ " in "
  ^ test_optional_arguments_complex_call
;;

let test_optional_arguments_complex_expected =
  let f ?(x = 0) ?(y = 0) () ?(z = 0) () = x + y + z in
  f ~x:5
;;

(* (8) Labeled arguments swapped places *)
let test_labeled_arguments_swapped_places_definition =
  "let f ~name2 ~name1 = name1 / name2"
;;

let test_labeled_arguments_swapped_places_call = "f ~name2:4 ~name1:5"

let test_labeled_arguments_swapped_places_expression =
  test_labeled_arguments_swapped_places_definition
  ^ " in "
  ^ test_labeled_arguments_swapped_places_call
;;

let test_labeled_arguments_swapped_places_expected =
  let f ~name2 ~name1 = name1 / name2 in
  f ~name2:4 ~name1:5
;;

(* ------------------------------------------------------ *)
(* -------------------- Parser tests -------------------- *)
(* ------------------------------------------------------ *)

open Parser

(* -------------------- Combinators --------------------- *)

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

(* App combinator (f x y) *)
let%test _ = parse expr_parser "f x" = Ok (App (Var "f", ArgNoLabel, Var "x"))

let%test _ =
  parse expr_parser "f (x) (y)"
  = Ok (App (App (Var "f", ArgNoLabel, Var "x"), ArgNoLabel, Var "y"))
;;

(* See app_parser combinator *)
(* let%test _ =
  parse expr_parser "f x y"
  = Ok (App (App (Var "f", ArgNoLabel, Var "x"), ArgNoLabel, Var "y"))
;; *)

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

(* ------------- Combinations of combinators ---------------- *)

(* (1) Factorial *)
let%test _ =
  parse definition_parser test_factorial_definition
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
                      , App (Var "fact", ArgNoLabel, Binop (Minus, Var "n", Const (Int 1)))
                      ) ) )
          , Var "fact" ) )
;;

let%test _ =
  parse expr_parser test_factorial_call = Ok (App (Var "fact", ArgNoLabel, Const (Int 5)))
;;

let%test _ =
  parse expr_parser test_factorial_expression
  = Ok
      (LetRec
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
                     , App (Var "fact", ArgNoLabel, Binop (Minus, Var "n", Const (Int 1)))
                     ) ) )
         , App (Var "fact", ArgNoLabel, Const (Int 5)) ))
;;

(* (2) X into the power of Y *)
let%test _ =
  parse definition_parser test_x_power_y_definition
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
                              ( App (Var "pow", ArgNoLabel, Var "x")
                              , ArgNoLabel
                              , Binop (Minus, Var "y", Const (Int 1)) ) ) ) ) )
          , Var "pow" ) )
;;

let%test _ =
  parse expr_parser test_x_power_y_call
  = Ok (App (App (Var "pow", ArgNoLabel, Const (Int 4)), ArgNoLabel, Const (Int 5)))
;;

let%test _ =
  parse expr_parser test_x_power_y_expression
  = Ok
      (LetRec
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
                             ( App (Var "pow", ArgNoLabel, Var "x")
                             , ArgNoLabel
                             , Binop (Minus, Var "y", Const (Int 1)) ) ) ) ) )
         , App (App (Var "pow", ArgNoLabel, Const (Int 4)), ArgNoLabel, Const (Int 5)) ))
;;

(* (3) Increment *)
let%test _ =
  parse definition_parser test_increment_definition
  = Ok ("inc", Fun (ArgNoLabel, None, "x", Binop (Plus, Var "x", Const (Int 1))))
;;

let%test _ =
  parse expr_parser test_increment_call = Ok (App (Var "inc", ArgNoLabel, Const (Int 4)))
;;

let%test _ =
  parse expr_parser test_increment_expression
  = Ok
      (Let
         ( "inc"
         , Fun (ArgNoLabel, None, "x", Binop (Plus, Var "x", Const (Int 1)))
         , App (Var "inc", ArgNoLabel, Const (Int 4)) ))
;;

(* (4) Labeled arguments *)
let%test _ =
  parse definition_parser test_labeled_arguments_definition
  = Ok
      ( "f"
      , Fun
          ( ArgLabeled "name1"
          , None
          , "x"
          , Fun (ArgLabeled "name2", None, "y", Binop (Divide, Var "x", Var "y")) ) )
;;

let%test _ =
  parse expr_parser test_labeled_arguments_call
  = Ok
      (App
         ( App (Var "f", ArgLabeled "name1", Const (Int 4))
         , ArgLabeled "name2"
         , Const (Int 5) ))
;;

let%test _ =
  parse expr_parser test_labeled_arguments_expression
  = Ok
      (Let
         ( "f"
         , Fun
             ( ArgLabeled "name1"
             , None
             , "x"
             , Fun (ArgLabeled "name2", None, "y", Binop (Divide, Var "x", Var "y")) )
         , App
             ( App (Var "f", ArgLabeled "name1", Const (Int 4))
             , ArgLabeled "name2"
             , Const (Int 5) ) ))
;;

(* (5) Labeled arguments syntactic sugar *)
let%test _ =
  parse definition_parser test_labeled_arguments_sugar_definition
  = Ok
      ( "f"
      , Fun
          ( ArgLabeled "x"
          , None
          , ""
          , Fun (ArgLabeled "y", None, "", Binop (Plus, Var "x", Var "y")) ) )
;;

let%test _ =
  parse expr_parser test_labeled_arguments_sugar_call
  = Ok (App (App (Var "f", ArgLabeled "x", Const (Int 4)), ArgLabeled "y", Const (Int 5)))
;;

let%test _ =
  parse expr_parser test_labeled_arguments_sugar_expression
  = Ok
      (Let
         ( "f"
         , Fun
             ( ArgLabeled "x"
             , None
             , ""
             , Fun (ArgLabeled "y", None, "", Binop (Plus, Var "x", Var "y")) )
         , App
             (App (Var "f", ArgLabeled "x", Const (Int 4)), ArgLabeled "y", Const (Int 5))
         ))
;;

(* (6) Optional arguments *)

let%test _ =
  parse definition_parser test_optional_arguments_definition
  = Ok
      ( "f"
      , Fun
          ( ArgOptional "name"
          , Some (Const (Int 4))
          , "arg1"
          , Fun (ArgNoLabel, None, "arg2", Binop (Plus, Var "arg1", Var "arg2")) ) )
;;

let%test _ =
  parse expr_parser test_optional_arguments_call
  = Ok (App (Var "f", ArgNoLabel, Const (Int 5)))
;;

let%test _ =
  parse expr_parser test_optional_arguments_expression
  = Ok
      (Let
         ( "f"
         , Fun
             ( ArgOptional "name"
             , Some (Const (Int 4))
             , "arg1"
             , Fun (ArgNoLabel, None, "arg2", Binop (Plus, Var "arg1", Var "arg2")) )
         , App (Var "f", ArgNoLabel, Const (Int 5)) ))
;;

(* (7) Complex optional arguments *)
let%test _ =
  parse definition_parser test_optional_arguments_complex_definition
  = Ok
      ( "f"
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

let%test _ =
  parse expr_parser test_optional_arguments_complex_call
  = Ok (App (Var "f", ArgLabeled "x", Const (Int 5)))
;;

let%test _ =
  parse expr_parser test_optional_arguments_complex_expression
  = Ok
      (Let
         ( "f"
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
                             , Binop (Plus, Binop (Plus, Var "x", Var "y"), Var "z") ) )
                     ) ) )
         , App (Var "f", ArgLabeled "x", Const (Int 5)) ))
;;

(* (8) Labeled arguments swapped places *)
let%test _ =
  parse definition_parser test_labeled_arguments_swapped_places_definition
  = Ok
      ( "f"
      , Fun
          ( ArgLabeled "name2"
          , None
          , ""
          , Fun (ArgLabeled "name1", None, "", Binop (Divide, Var "name1", Var "name2"))
          ) )
;;

let%test _ =
  parse expr_parser test_labeled_arguments_swapped_places_call
  = Ok
      (App
         ( App (Var "f", ArgLabeled "name2", Const (Int 4))
         , ArgLabeled "name1"
         , Const (Int 5) ))
;;

let%test _ =
  parse expr_parser test_labeled_arguments_swapped_places_expression
  = Ok
      (Let
         ( "f"
         , Fun
             ( ArgLabeled "name2"
             , None
             , ""
             , Fun (ArgLabeled "name1", None, "", Binop (Divide, Var "name1", Var "name2"))
             )
         , App
             ( App (Var "f", ArgLabeled "name2", Const (Int 4))
             , ArgLabeled "name1"
             , Const (Int 5) ) ))
;;

(* ------------------------------------------------------ *)
(* -------------------- Infer tests --------------------- *)
(* ------------------------------------------------------ *)

(* To be done *)

(* ------------------------------------------------------ *)
(* --------------------- Eval tests --------------------- *)
(* ------------------------------------------------------ *)

open Interpret
open Interpret (EvalResult)

(* let basic = Binop (Plus, Const (Int 1), Const (Int 9))
let increment = Fun (ArgNoLabel, None, "x", Binop (Plus, Var "x", Const (Int 1)))

let%test _ =
  let env = IdMap.empty in
  match eval basic env with
  | Ok (VInt 10) -> true
  | _ -> false
;;

let%test _ =
  let env = IdMap.add "x" (ref (VInt 8)) IdMap.empty in
  match eval increment env with
  | Ok (VClosure (_, ArgNoLabel, None, "x", Binop (Plus, Var "x", Const (Int 1)))) -> true
  | _ -> false
;; *)

let test_parse_and_eval_single_expr_ok
  test_case
  ?(env : environment = IdMap.empty)
  expected_value
  =
  let test_inner toplevel =
    match toplevel with
    | Definition _ ->
      Format.printf
        "Definitions are for the REPL. Test expression evaluation instead!\n%!";
      false
    | Expression exp ->
      let value = eval exp env in
      (match value with
       | Result.Ok v ->
         if compare_values v expected_value = Result.Ok 0 then true else false
       | Result.Error e ->
         Prettyprint.pp_error Format.std_formatter e;
         false)
    | Command _ ->
      Format.printf "Commands are for the REPL. Test expression evaluation instead!\n%!";
      false
  in
  match Parser.parse_toplevel test_case with
  | Error e ->
    Format.printf "%s" e;
    false
  | Result.Ok toplevel_input ->
    (match toplevel_input with
     | [ h ] -> test_inner h
     | _ ->
       Format.printf "Use this function to test single expressions\n%!";
       false)
;;

let%test _ =
  test_parse_and_eval_single_expr_ok
    test_factorial_expression
    (VInt test_factorial_expected)
;;

let%test _ =
  test_parse_and_eval_single_expr_ok
    test_x_power_y_expression
    (VInt test_x_power_y_expected)
;;

let%test _ =
  test_parse_and_eval_single_expr_ok
    test_increment_expression
    (VInt test_increment_expected)
;;

let%test _ =
  test_parse_and_eval_single_expr_ok
    test_labeled_arguments_expression
    (VInt test_labeled_arguments_expected)
;;

let%test _ =
  test_parse_and_eval_single_expr_ok
    test_labeled_arguments_sugar_expression
    (VInt test_labeled_arguments_sugar_expected)
;;

let%test _ =
  test_parse_and_eval_single_expr_ok
    test_optional_arguments_expression
    (VInt test_optional_arguments_expected)
;;

(* let%test _ =
  test_parse_and_eval_single_expr_ok
    test_optional_arguments_complex_expression
    (VInt test_optional_arguments_complex_expected)
;; *)

let%test _ =
  test_parse_and_eval_single_expr_ok
    test_labeled_arguments_swapped_places_expression
    (VInt test_labeled_arguments_swapped_places_expected)
;;

(* ------------------------------------------------------ *)
(* --------------------- REPL tests --------------------- *)
(* ------------------------------------------------------ *)

open Repl

let test_repl_ok test_case (env : environment) =
  match Parser.parse_toplevel test_case with
  | Error e ->
    Format.printf "%s" e;
    false
  | Result.Ok toplevel_input ->
    let rec helper env toplevel_input =
      match toplevel_input with
      | [] -> IdMap.empty
      | [ h ] -> repl env h
      | h :: tl -> helper (repl env h) tl
    in
    let actual_env = helper env toplevel_input in
    Format.printf "\n\n----- Test case -----\n";
    Format.printf "%s" test_case;
    Format.printf "\n\n----- Environment -----\n";
    Prettyprint.pp_env Format.std_formatter actual_env;
    false
;;

(* let%test _ =
  test_repl_ok
    (test_factorial_definition ^ "\n;;\n" ^ test_factorial_call)
    IdMap.empty
;; *)

(* let%test _ =
  test_repl_ok
    (test_increment_definition ^ "\n;;\n" ^ test_increment_call)
    IdMap.empty
;; *)

(* let%test _ =
  test_repl_ok
    (test_x_power_y_definition ^ "\n;;\n" ^ test_x_power_y_call)
    IdMap.empty
;; *)

(* let%test _ =
  test_repl_ok
    (test_labeled_arguments_definition ^ "\n;;\n" ^ test_labeled_arguments_call)
    IdMap.empty
;; *)

(* let%test _ =
  test_repl_ok
    (test_optional_arguments_definition ^ "\n;;\n" ^ test_optional_arguments_call)
    IdMap.empty
;; *)