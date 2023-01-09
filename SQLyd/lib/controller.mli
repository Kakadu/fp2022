(** Copyright 2022-2023, Danil Yevdokimov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** A controller that executes SQL operations*)

open Type

(* Create a table. *)
val create : database -> string -> string list -> data_type list -> database

(* Select some entries in a table based on a filter function. *)
val select
  :  database
  -> string
  -> string list
  -> (string list * string list -> bool)
  -> unit

(* Insert one entry into the table. *)
val insert : database -> string -> string list -> terminal list -> database

(* Delete some entries in a table based on a filter function. *)
val delete : database -> string -> (string list * string list -> bool) -> database

(* Update some entires in a table based on a fileter function. *)
val update
  :  database
  -> string
  -> string list
  -> terminal list
  -> (string list * string list -> bool)
  -> database

(* Drops the table [table_name] from the database. *)
val drop : database -> string -> database

(* Saves the proposed table to a csv file. *)
val save : database -> string -> unit

(* Read the csv file to a database. *)
val read : database -> string -> database
