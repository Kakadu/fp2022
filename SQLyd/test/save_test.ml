(** Copyright 2022-2023, Danil Yevdokimov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open SQLyd_lib.Type
open SQLyd_lib.Tree
open SQLyd_lib.Save

let class_number_tree =
  let insert_1 = insert (0, "1110") empty in
  let insert_2 = insert (1, "2110") insert_1 in
  let insert_3 = insert (2, "2800") insert_2 in
  insert_3
;;

let class_name_tree =
  let insert_1 = insert (0, "Intro to Python") empty in
  let insert_2 = insert (1, "Data Structures") insert_1 in
  let insert_3 = insert (2, "Discrete Structures") insert_2 in
  insert_3
;;

let professor_name_tree =
  let insert_1 = insert (0, "Walker White") empty in
  let insert_2 = insert (1, "David Gries") insert_1 in
  let insert_3 = insert (2, "Anke") insert_2 in
  insert_3
;;

let class_level_tree =
  let insert_1 = insert (0, "1") empty in
  let insert_2 = insert (1, "2") insert_1 in
  let insert_3 = insert (2, "2") insert_2 in
  insert_3
;;

let class_column_0 = { field_name = "Number"; data_type = Int; data = class_number_tree }
let class_column_1 = { field_name = "Name"; data_type = String; data = class_name_tree }

let class_column_2 =
  { field_name = "Professor"; data_type = String; data = professor_name_tree }
;;

let class_column_3 =
  { field_name = "Class Level"; data_type = Int; data = class_level_tree }
;;

let class_column_tree =
  let insert_0 = insert (0, class_column_0) empty in
  let insert_1 = insert (1, class_column_1) insert_0 in
  let insert_2 = insert (2, class_column_2) insert_1 in
  let insert_3 = insert (3, class_column_3) insert_2 in
  insert_3
;;

let class_table = { table_name = "classes"; columns = class_column_tree; num_rows = 3 }

let name_tree =
  let insert_1 = insert (0, "Yolanda") empty in
  let insert_2 = insert (1, "Lee") insert_1 in
  let insert_3 = insert (2, "Emerald") insert_2 in
  insert_3
;;

let major_tree =
  let insert_1 = insert (0, "CS/Info") empty in
  let insert_2 = insert (1, "Math") insert_1 in
  let insert_3 = insert (2, "CS") insert_2 in
  insert_3
;;

let age_tree =
  let insert_1 = insert (0, "18") empty in
  let insert_2 = insert (1, "19") insert_1 in
  let insert_3 = insert (2, "19") insert_2 in
  insert_3
;;

let student_column_0 = { field_name = "Name"; data_type = String; data = name_tree }
let student_column_1 = { field_name = "Major"; data_type = String; data = major_tree }
let student_column_2 = { field_name = "Age"; data_type = Int; data = age_tree }

let student_column_tree =
  let insert_0 = insert (0, student_column_0) empty in
  let insert_1 = insert (1, student_column_1) insert_0 in
  let insert_2 = insert (2, student_column_2) insert_1 in
  insert_2
;;

let student_table =
  { table_name = "students"; columns = student_column_tree; num_rows = 3 }
;;

let language_tree =
  let insert_1 = insert (0, "Java") empty in
  let insert_2 = insert (1, "Python") insert_1 in
  let insert_3 = insert (2, "OCaml") insert_2 in
  insert_3
;;

let inventor_tree =
  let insert_1 = insert (0, "James Gosling") empty in
  let insert_2 = insert (1, "Rossum") insert_1 in
  let insert_3 = insert (2, "Leroy") insert_2 in
  insert_3
;;

let year_tree =
  let insert_1 = insert (0, "1991") empty in
  let insert_2 = insert (1, "1992") insert_1 in
  let insert_3 = insert (2, "1996") insert_2 in
  insert_3
;;

let programming_column_0 =
  { field_name = "Language"; data_type = String; data = language_tree }
;;

let programming_column_1 =
  { field_name = "Inventor"; data_type = String; data = inventor_tree }
;;

let programming_column_2 =
  { field_name = "Year Invented"; data_type = Int; data = year_tree }
;;

let programming_column_tree =
  let insert_0 = insert (0, programming_column_0) empty in
  let insert_1 = insert (1, programming_column_1) insert_0 in
  let insert_2 = insert (2, programming_column_2) insert_1 in
  insert_2
;;

let programming_table =
  { table_name = "programming"; columns = programming_column_tree; num_rows = 3 }
;;

let table_tree =
  let insert_1 = insert (0, class_table) empty in
  let insert_2 = insert (1, student_table) insert_1 in
  let insert_3 = insert (2, programming_table) insert_2 in
  insert_3
;;

let test_db = { database_name = "parent"; tables = table_tree; num_tables = 3 }

let suite =
  save_file test_db "programming";
  save_file test_db "classes";
  save_file test_db "students"
;;
