(** Copyright 2022-2023, Denis Porsev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** [Tests] contains most test written for our mini-language.*)

val test_factorial_definition : string
val test_factorial_call : string
val test_factorial_expression : string
val test_factorial_expected : int
val test_x_power_y_definition : string
val test_x_power_y_call : string
val test_x_power_y_expression : string
val test_x_power_y_expected : int
val test_increment_definition : string
val test_increment_call : string
val test_increment_expression : string
val test_increment_expected : int
val test_labeled_arguments_definition : string
val test_labeled_arguments_call : string
val test_labeled_arguments_expression : string
val test_labeled_arguments_expected : int
val test_labeled_arguments_sugar_definition : string
val test_labeled_arguments_sugar_call : string
val test_labeled_arguments_sugar_expression : string
val test_labeled_arguments_sugar_expected : int
val test_optional_arguments_definition : string
val test_optional_arguments_call : string
val test_optional_arguments_expression : string
val test_optional_arguments_expected : int
val test_optional_arguments_complex_definition : string
val test_optional_arguments_complex_call : string
val test_optional_arguments_complex_expression : string
val test_optional_arguments_complex_expected : int
val test_labeled_arguments_swapped_places_definition : string
val test_labeled_arguments_swapped_places_call : string
val test_labeled_arguments_swapped_places_expression : string
val test_labeled_arguments_swapped_places_expected : int
val test_mult_arguments_definition : string

val test_parse_and_eval_single_expr_ok
  :  string
  -> ?env:Typedtree.environment
  -> Typedtree.value
  -> bool
