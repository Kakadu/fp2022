(** Copyright 2022-2023, Danil Yevdokimov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Internal representation of the database *)

open Type

exception Internal of string
exception WrongTableStructure
exception WrongDBStructure
exception WrongType
exception IllegalName
exception ColumnDNE
exception TableDNE

val default_int : int
val default_float : float
val default_string : string
val get_row_num : table -> int
val get_col_num : table -> int

(* returns list of column names and datatypes *)
val get_field_name_list : database -> string -> (string * data_type) list

(* returns list of data in this column as string list *)
val get_column_data : database -> string -> string -> string list
val create_table : database -> string -> (string * data_type) list -> database
val create_empty_database : string -> database

(* returns a table only with rows that satisfy the condition *)
val select
  :  database
  -> string
  -> string list
  -> (string list * string list -> bool)
  -> table

(* returns a table with one row inserted, 
  the columns that is specified in the function have
  customized values The rest of the columns get default values *)
val insert_row : database -> string -> (string * string) list -> database
val delete_row : database -> string -> (string list * string list -> bool) -> database

val update_row
  :  database
  -> string
  -> (string * string) list
  -> (string list * string list -> bool)
  -> database

val drop_table : database -> string -> database
val pretty_print : table -> string
