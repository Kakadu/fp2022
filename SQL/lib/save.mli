(** Copyright 2022-2023, Danil Yevdokimov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Type

(* save all current datas in table in database to a file *)
val save_file : database -> string -> unit

(* convert table in csv format back into a table in database *)
val file_to_db : database -> string -> database
