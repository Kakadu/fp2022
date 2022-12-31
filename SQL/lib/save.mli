open Type

(* save all current datas in table in database to a file *)
val save_file : database -> string -> unit

(* convert table in csv format back into a table in database *)
val file_to_db : database -> string -> database
