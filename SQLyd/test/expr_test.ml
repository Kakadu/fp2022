(** Copyright 2022-2023, Danil Yevdokimov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OUnit2
open SQLyd_lib.Parser
open SQLyd_lib.Type

(* (A = 1) or (D = 4) *)
let (tokens1 : expr_type list) =
  [ String "A"; EQ; String "1"; OR; String "D"; EQ; String "4" ]
;;

let tokens1_or = [ [ String "A"; EQ; String "1" ]; [ String "D"; EQ; String "4" ] ]

(* (A = 1 and D = 4 and C >= 3) *)
let (tokens2 : expr_type list) =
  [ String "A"
  ; EQ
  ; String "1"
  ; AND
  ; String "B"
  ; EQ
  ; String "2"
  ; AND
  ; String "C"
  ; LE
  ; String "3"
  ]
;;

let tokens2_or =
  [ [ String "A"
    ; EQ
    ; String "1"
    ; AND
    ; String "B"
    ; EQ
    ; String "2"
    ; AND
    ; String "C"
    ; LE
    ; String "3"
    ]
  ]
;;

(* (A = 1) or ( B > 2 and C >= 3 ) or ( NOT D = 4 ) *)
let (tokens3 : expr_type list) =
  [ String "A"
  ; EQ
  ; String "1"
  ; OR
  ; String "B"
  ; GT
  ; String "2"
  ; AND
  ; String "C"
  ; LE
  ; String "3"
  ; OR
  ; String "D"
  ; EQ
  ; String "4"
  ]
;;

let tokens3_or =
  [ [ String "A"; EQ; String "1" ]
  ; [ String "B"; GT; String "2"; AND; String "C"; LE; String "3" ]
  ; [ String "D"; EQ; String "4" ]
  ]
;;

let or_tests =
  [ ("token1" >:: fun _ -> assert_equal (expressions_or tokens1) tokens1_or)
  ; ("token2" >:: fun _ -> assert_equal (expressions_or tokens2) tokens2_or)
  ; ("token3" >:: fun _ -> assert_equal (expressions_or tokens3) tokens3_or)
  ]
;;

let a = String "A"
let b = String "B"
let c = String "C"
let d = String "D"
let one = Int 1
let two = Int 2
let three = Int 3
let four = Int 4
let one = Int 1
let pair_list = [ a, one; b, two; c, three; d, four ]

let and_test name expected actual =
  name >:: fun _ -> assert_equal expected actual ~printer:string_of_bool
;;

let and_tests =
  [ and_test "EQ_true" true (and_condition_evaluater a EQ one pair_list)
  ; and_test "EQ_false" false (and_condition_evaluater a EQ two pair_list)
  ; and_test "NE_true" true (and_condition_evaluater a NE b pair_list)
  ; and_test "NE_false" false (and_condition_evaluater b NE two pair_list)
  ; and_test "LE_true" true (and_condition_evaluater c LE three pair_list)
  ; and_test "LE_false" false (and_condition_evaluater c LE two pair_list)
  ; and_test "GE_true" true (and_condition_evaluater d GE two pair_list)
  ; and_test "GE_fale" false (and_condition_evaluater a GE two pair_list)
  ; and_test "GT_true" true (and_condition_evaluater d GT three pair_list)
  ; and_test "GT_false" false (and_condition_evaluater d GT four pair_list)
  ; and_test "LT_true" true (and_condition_evaluater c LT four pair_list)
  ; and_test "LT_false" false (and_condition_evaluater c LT three pair_list)
  ]
;;

(* true, first true *)
let or_lst1 = [ [ a; EQ; one; AND; b; EQ; two ]; [ c; EQ; four ] ]

(* false *)
let or_lst2 = [ [ a; NE; one; AND; b; NE; two ]; [ c; EQ; four ] ]

(* true, second true *)
let or_lst3 = [ [ a; NE; one; AND; b; NE; two ]; [ c; EQ; three ] ]

(* true, first true *)
let or_lst4 =
  [ [ a; EQ; one; AND; b; GT; one; AND; c; LE; three; d; GT; three ]
  ; [ a; NE; one ]
  ; [ b; NE; two ]
  ; [ c; GT; three ]
  ]
;;

(* false *)
let or_lst5 =
  [ [ a; NE; one; AND; b; GT; one; AND; c; LE; three; d; GT; three ]
  ; [ a; NE; one ]
  ; [ b; NE; two ]
  ; [ c; GT; three ]
  ]
;;

let multiple_and_test name expected actual =
  name >:: fun _ -> assert_equal expected actual ~printer:string_of_bool
;;

let multiple_and_tests =
  [ multiple_and_test "two_and_true1" true (evaluate_or or_lst1 pair_list)
  ; multiple_and_test "two_and_false1" false (evaluate_or or_lst2 pair_list)
  ; multiple_and_test "two_and_true2" true (evaluate_or or_lst3 pair_list)
  ; multiple_and_test "three_and_true" true (evaluate_or or_lst4 pair_list)
  ; multiple_and_test "three_and_false" false (evaluate_or or_lst5 pair_list)
  ]
;;

let suite =
  "test suite for expression tree"
  >::: List.flatten [ or_tests; and_tests; multiple_and_tests ]
;;

let _ = run_test_tt_main suite
