(** Copyright 2022-2023, Danil Yevdokimov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OUnit2
open SQLyd_lib.Tokenizer
open SQLyd_lib.Parser
open SQLyd_lib.Type
open SQLyd_lib.Helper

(* (Country = Mexico) or (LandSize >= 1000 and Population >= 1000) *)
let condition1 =
  [ Terminal (String "Country")
  ; BinaryOp EQ
  ; Terminal (String "Mexico")
  ; LogicOp OR
  ; Terminal (String "LandSize")
  ; BinaryOp GE
  ; Terminal (Int 1000)
  ; LogicOp AND
  ; Terminal (String "Population")
  ; BinaryOp GE
  ; Terminal (Int 1000)
  ]
;;

(* (Country = Mexico) or (LandSize < 1000 and Population < 1000) *)
let condition2 =
  [ Terminal (String "Country")
  ; BinaryOp EQ
  ; Terminal (String "Mexico")
  ; LogicOp OR
  ; Terminal (String "LandSize")
  ; BinaryOp LT
  ; Terminal (Int 1000)
  ; LogicOp AND
  ; Terminal (String "Population")
  ; BinaryOp LT
  ; Terminal (Int 1000)
  ]
;;

(* Country != Mexico *)
let condition3 = [ Terminal (String "Country"); BinaryOp NE; Terminal (String "Mexico") ]

let condition4 =
  [ Terminal (String "Country"); BinaryOp EQ; Terminal (String "Neverland") ]
;;

let condition5 =
  [ Terminal (String "LandSize")
  ; BinaryOp EQ
  ; Terminal (Int 10000)
  ; LogicOp OR
  ; Terminal (String "LandSize")
  ; BinaryOp EQ
  ; Terminal (Int 5000)
  ; LogicOp OR
  ; Terminal (String "LandSize")
  ; BinaryOp EQ
  ; Terminal (Int 100)
  ]
;;

let condition6 =
  [ Terminal (String "LandSize")
  ; BinaryOp EQ
  ; Terminal (Int 10001)
  ; LogicOp OR
  ; Terminal (String "LandSize")
  ; BinaryOp EQ
  ; Terminal (Int 5001)
  ; LogicOp OR
  ; Terminal (String "LandSize")
  ; BinaryOp EQ
  ; Terminal (Int 101)
  ]
;;

let pair_list_title = [ "Language"; "Country"; "LandSize"; "Continent"; "Population" ]

(* condition1 true; condition2 false; *)
let pair_list_China = pair_list_title, [ "Chinese"; "China"; "10000"; "Asia"; "1000" ]

(* condition1 false; condition2 false; *)
let pair_list_USA = pair_list_title, [ "English"; "USA"; "5000"; "NorthAmerica"; "500" ]

(* condition1 true; condition2 true; *)
let pair_list_Mexico =
  pair_list_title, [ "Mexcian"; "Mexico"; "100"; "NorthAmerica"; "100" ]
;;

let parse_where_test name expected actual =
  name >:: fun _ -> assert_equal expected actual ~printer:string_of_bool
;;

let parse_where_tests =
  [ parse_where_test "condition1_USA" false (parse_where condition1 pair_list_USA)
  ; parse_where_test "condition1_China" true (parse_where condition1 pair_list_China)
  ; parse_where_test "condition1_Mexico" true (parse_where condition1 pair_list_Mexico)
  ; parse_where_test "condition2_USA" false (parse_where condition2 pair_list_USA)
  ; parse_where_test "condition2_China" false (parse_where condition2 pair_list_China)
  ; parse_where_test "condition2_Mexico" true (parse_where condition2 pair_list_Mexico)
  ; parse_where_test "condition3_USA" true (parse_where condition3 pair_list_USA)
  ; parse_where_test "condition3_China" true (parse_where condition3 pair_list_China)
  ; parse_where_test "condition3_Mexico" false (parse_where condition3 pair_list_Mexico)
  ; parse_where_test "condition4_USA" false (parse_where condition4 pair_list_USA)
  ; parse_where_test "condition4_China" false (parse_where condition4 pair_list_China)
  ; parse_where_test "condition4_Mexico" false (parse_where condition4 pair_list_Mexico)
  ; parse_where_test "condition5_USA" true (parse_where condition5 pair_list_USA)
  ; parse_where_test "condition5_China" true (parse_where condition5 pair_list_China)
  ; parse_where_test "condition5_Mexico" true (parse_where condition5 pair_list_Mexico)
  ; parse_where_test "condition6_USA" false (parse_where condition6 pair_list_USA)
  ; parse_where_test "condition6_China" false (parse_where condition6 pair_list_China)
  ; parse_where_test "condition6_Mexico" false (parse_where condition6 pair_list_Mexico)
  ]
;;

type t = string * string list * terminal list

(** helper: return the list with the head removed *)
let remove_hd = function
  | h :: t -> t
  | _ -> []
;;

let update_token_1 =
  remove_hd
    (tokenize
       "UPDATE Customers SET ContactName = 'AlfredSchmidt', City = 'Frankfurt' WHERE \
        CustomerID = 1 ;")
;;

let update_token_2 =
  [ Terminal (String "Customers")
  ; SubCommand Set
  ; Terminal (String "ContactName")
  ; BinaryOp EQ
  ; Terminal (Float 6.2)
  ; Terminal (String "City")
  ; BinaryOp EQ
  ; Terminal (String "'Frankfurt',")
  ; Terminal (String "Address")
  ; BinaryOp EQ
  ; Terminal (Int 9)
  ; SubCommand Where
  ; Terminal (String "CustomerID")
  ; BinaryOp EQ
  ; Terminal (Int 1)
  ; EndOfQuery EOQ
  ]
;;

let get_list_after_where_test
  (name : string)
  (tokens : token list)
  (expected_output : token list)
  : test
  =
  name
  >:: fun _ ->
  assert_equal expected_output (tokens |> get_this_command |> get_list_after_where)
;;

let get_list_after_where_tests =
  [ get_list_after_where_test
      "command1"
      update_token_1
      [ Terminal (String "CustomerID"); BinaryOp EQ; Terminal (Int 1) ]
  ; get_list_after_where_test
      "command2"
      update_token_2
      [ Terminal (String "CustomerID"); BinaryOp EQ; Terminal (Int 1) ]
  ]
;;

let long_command =
  "INTO Customers (CustomerName, ContactName, Address, PostalCode, Country) VALUES \
   ('Cardinal', 'TomErichsen', 'Skagen21', 'Stavanger', '4006', 'Norway') ;  UPDATE \
   Customers SET ContactName = 'Alfred Schmidt', City = 'Frankfurt', Address = 9.0 WHERE \
   CustomerID = 1 ; DELETE FROM Customers WHERE CustomerID = 1 ; "
  |> tokenize
;;

let get_this_command_test
  (name : string)
  (tokens : token list)
  (expected_output : token list)
  : test
  =
  name >:: fun _ -> assert_equal expected_output (tokens |> get_this_command)
;;

let get_this_command_tests =
  [ get_this_command_test
      "complex command"
      long_command
      [ SubCommand Into
      ; Terminal (String "Customers")
      ; Terminal (String "(CustomerName,")
      ; Terminal (String "ContactName,")
      ; Terminal (String "Address,")
      ; Terminal (String "PostalCode,")
      ; Terminal (String "Country)")
      ; SubCommand Values
      ; Terminal (String "('Cardinal',")
      ; Terminal (String "'TomErichsen',")
      ; Terminal (String "'Skagen21',")
      ; Terminal (String "'Stavanger',")
      ; Terminal (String "'4006',")
      ; Terminal (String "'Norway')")
      ]
  ]
;;

let get_other_command_test
  (name : string)
  (tokens : token list)
  (expected_output : token list)
  : test
  =
  name >:: fun _ -> assert_equal expected_output (tokens |> get_other_commands)
;;

let get_other_commands_tests =
  [ get_other_command_test
      "complex command"
      long_command
      [ Command Update
      ; Terminal (String "Customers")
      ; SubCommand Set
      ; Terminal (String "ContactName")
      ; BinaryOp EQ
      ; Terminal (String "'Alfred")
      ; Terminal (String "Schmidt',")
      ; Terminal (String "City")
      ; BinaryOp EQ
      ; Terminal (String "'Frankfurt',")
      ; Terminal (String "Address")
      ; BinaryOp EQ
      ; Terminal (Float 9.)
      ; SubCommand Where
      ; Terminal (String "CustomerID")
      ; BinaryOp EQ
      ; Terminal (Int 1)
      ; EndOfQuery EOQ
      ; Command Delete
      ; SubCommand From
      ; Terminal (String "Customers")
      ; SubCommand Where
      ; Terminal (String "CustomerID")
      ; BinaryOp EQ
      ; Terminal (Int 1)
      ; EndOfQuery EOQ
      ]
  ]
;;

let suite =
  "test suite for expression tree"
  >::: List.flatten
         [ parse_where_tests
         ; get_list_after_where_tests
         ; get_this_command_tests
         ; get_other_commands_tests
         ]
;;

let _ = run_test_tt_main suite
