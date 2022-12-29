(** Copyright 2021-2022, Ilya Shchuckin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Contains some tests for the parser*)

open Parser
open Angstrom
open Ast

let expt_test pp parser str =
  match parse_string ~consume:Consume.All parser str with
  | Ok x -> Format.printf "%a" pp (Result.get_ok (Ok x))
  | Error _ -> Format.printf "Parsing error"
;;

let%expect_test _ =
  expt_test pp_atom name "cat";
  [%expect {| (Name "cat") |}]
;;

let%expect_test _ =
  expt_test pp_atom name "A";
  [%expect {| Parsing error |}]
;;

let%expect_test _ =
  expt_test pp_term term "cat";
  [%expect {| Parsing error |}]
;;

let%expect_test _ =
  expt_test pp_term term "cat(tom).";
  [%expect {| Compound {atom = (Name "cat"); terms = [(Atomic (Atom (Name "tom")))]} |}]
;;

let%expect_test _ =
  expt_test pp_term term "s([a, b], Ys).";
  [%expect
    {| 

Compound {atom = (Name "s");
  terms =
  [Compound {atom = (Name ".");
     terms =
     [(Atomic (Atom (Name "a")));
       Compound {atom = (Name ".");
         terms = [(Atomic (Atom (Name "b"))); (Atomic (Atom (Name "[]")))]}
       ]};
    (Var "Ys")]}
  |}]
;;

let%expect_test _ =
  expt_test pp_term term "a(X)    :-  b(X), c(_).";
  [%expect
    {| 
Compound {atom = (Operator ":-");
  terms =
  [Compound {atom = (Name "a"); terms = [(Var "X")]};
    Compound {atom = (Operator ",");
      terms =
      [Compound {atom = (Name "b"); terms = [(Var "X")]};
        Compound {atom = (Name "c"); terms = [(Var "_")]}]}
    ]} |}]
;;

let%expect_test _ =
  expt_test pp_term term "prove((B, Bs)).";
  [%expect
    {|
Compound {atom = (Name "prove");
  terms = [Compound {atom = (Operator ","); terms = [(Var "B"); (Var "Bs")]}]}|}]
;;

let%expect_test _ =
  expt_test pp_term term "A == B.";
  [%expect {| Compound {atom = (Operator "=="); terms = [(Var "A"); (Var "B")]} |}]
;;

let%expect_test _ =
  expt_test pp_term term "A :- B, C.";
  [%expect
    {|
      Compound {atom = (Operator ":-");
        terms =
        [(Var "A");
          Compound {atom = (Operator ","); terms = [(Var "B"); (Var "C")]}]} |}]
;;

let%expect_test _ =
  expt_test pp_term term "A :- [].";
  [%expect
    {|
      Compound {atom = (Operator ":-");
        terms = [(Var "A"); (Atomic (Atom (Name "[]")))]} |}]
;;

let%expect_test _ =
  expt_test pp_term term "A :- A == B, !.";
  [%expect
    {|
      Compound {atom = (Operator ":-");
        terms =
        [(Var "A");
          Compound {atom = (Operator ",");
            terms =
            [Compound {atom = (Operator "=="); terms = [(Var "A"); (Var "B")]};
              (Atomic (Atom (Name "!")))]}
          ]} |}]
;;

let%expect_test _ =
  expt_test pp_term term "A :- A == B,\n  \n  \n  !.";
  [%expect
    {|
      Compound {atom = (Operator ":-");
        terms =
        [(Var "A");
          Compound {atom = (Operator ",");
            terms =
            [Compound {atom = (Operator "=="); terms = [(Var "A"); (Var "B")]};
              (Atomic (Atom (Name "!")))]}
          ]} |}]
;;

(* let test_ok, test_fail =
  let ok ppf parser input =
    match parse_string ~consume:All parser input with
    | Ok res ->
      ppf Format.std_formatter res;
      false
    | Error e ->
      print_string e;
      false
  in
  let fail ppf parser input =
    match parse_string ~consume:All parser input with
    | Ok res ->
      ppf Format.std_formatter res;
      false
    | _ -> true
  in
  ok, fail
;; *)

(* let parse_ast_subtree parser str =
  match parse_string ~consume:Consume.All parser str with
  | Ok x -> Ok x
  | Error er -> Error (`ParsingError er)
;;

(* let print_test = test_ok pp_program parse_prolog *)
let print_ast ppf parser input =
  match parse_string ~consume:All parser input with
  | Ok res ->
    ppf Format.std_formatter res;
    true
  | Error er ->
    print_string er;
    false
;; *)

(* let%test _ = print_ast pp_program parse_prolog "prove(true) :- !." *)
(* let%test _ = print_ast pp_program parse_prolog "Answer == yes." *)
