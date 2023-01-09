(** Copyright 2022-2023, Danil Yevdokimov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Tree
open Type
open Helper

exception Internal of string
exception WrongTableStructure
exception WrongDBStructure
exception WrongType
exception IllegalName
exception ColumnDNE
exception TableDNE
exception Duplicate

let default_int = 0
let default_float = 0.
let default_string = "NaN"
let int_of_string s = if s = "NaN" then 0 else Stdlib.int_of_string s
let float_of_string s = if s = "NaN" then 0. else Stdlib.float_of_string s

let tree_find (filter : 'a -> bool) (tree : 'a tree) =
  let filtered_tree = filter_based_on_value filter tree in
  if size filtered_tree = 1
  then get 0 (update_key filtered_tree)
  else if size filtered_tree = 0
  then raise Not_found
  else raise Duplicate
;;

(* placeholder for single database implementation *)
let parent_db = { database_name = "parent"; tables = empty; num_tables = 0 }
let get_field_name { field_name } = field_name
let get_data_type { data_type } = data_type

(* assoc list of field names and datatypes *)
let get_field_name_list_internal table =
  let rec extract_list = function
    | [] -> []
    | h :: t -> (get_field_name (snd h), get_data_type (snd h)) :: extract_list t
  in
  extract_list (inorder table.columns)
;;

let get_tablename_list_internal database =
  let rec extract_list = function
    | [] -> []
    | (_, { table_name }) :: t -> table_name :: extract_list t
  in
  extract_list (inorder database.tables)
;;

let get_field_name_list database table_name =
  try
    let table = tree_find (fun x -> x.table_name = table_name) database.tables in
    get_field_name_list_internal table
  with
  | Not_found -> raise TableDNE
;;

let get_table_name_internal { table_name } = table_name
let get_database_name_internal { database_name } = database_name

(* list of data in provided column *)
let get_column_data_internal = function
  | { field_name; data } -> data
;;

let get_column_data database table_name field_name =
  let table = tree_find (fun x -> x.table_name = table_name) database.tables in
  let column = tree_find (fun x -> x.field_name = field_name) table.columns in
  let data = get_column_data_internal column in
  to_value_list data
;;

let get_row_num { table_name; columns; num_rows } = num_rows
let get_col_num table = size table.columns
let get_table_num db = db.num_tables

(* gets the cell in this column whose index matches the row_num *)
let get_one_cell (column : column) (row_num : int) : string =
  let data = get_column_data_internal column in
  get row_num data
;;

(* data in this row as a list *)
let get_one_row (db : database) (table_name : string) (row_num : int) : string list =
  let table = tree_find (fun table -> table.table_name = table_name) db.tables in
  let all_index_and_columns = inorder table.columns in
  let all_columns = List.map (fun x -> snd x) all_index_and_columns in
  List.map (fun col -> get_one_cell col row_num) all_columns
;;

(* list [0,1,2...(num_rows - 1)] *)
let get_list_of_row_numbers (table : table) : int list =
  let num_rows = get_row_num table in
  range num_rows
;;

open Format

let get_all_rows table =
  try
    let num_rows = table.num_rows in
    let row_list = range num_rows in
    List.map
      (fun row ->
        String.concat
          ""
          [ "| "
          ; String.concat
              " | "
              (let all_columns = List.map (fun x -> snd x) (inorder table.columns) in
               List.map (fun col -> get_one_cell col row) all_columns)
          ; " |"
          ])
      row_list
  with
  | Not_found -> raise TableDNE
;;

let string_of_data_type = function
  | (String : data_type) -> "String"
  | (Int : data_type) -> "Int"
  | (Float : data_type) -> "Float"
;;

let longest_field_length table =
  let pair_list = List.split (get_field_name_list_internal table) in
  let string_list =
    fst pair_list @ List.map (fun x -> string_of_data_type x) (snd pair_list)
  in
  let string_length_list = List.map (fun x -> String.length x) string_list in
  let max_length = max string_length_list in
  max_length
;;

let rec repeat n s = if n = 0 then "" else s ^ repeat (n - 1) s

(* concatenate spaces to the end of the str till desired length *)
let buff_up_string str desired_length =
  let original_length = String.length str in
  let difference = desired_length - original_length in
  str ^ repeat difference " "
;;

let buff_up_string_list str_lst desired_length =
  List.map (fun x -> buff_up_string x desired_length) str_lst
;;

(* gets the cell in this column whose index matches the row_num and buff it up to desired length *)
let get_one_cell_even_length (column : column) (row_num : int) (desired_length : int)
  : string
  =
  let original =
    let data = get_column_data_internal column in
    get row_num data
  in
  buff_up_string original desired_length
;;

(* get all rows but each cell is filled to the longest length *)
let get_all_rows_even_length table longest_length =
  try
    let num_rows = table.num_rows in
    let row_list = range num_rows in
    print_int num_rows;
    List.map
      (fun row ->
        String.concat
          ""
          [ "| "
          ; String.concat
              " | "
              (let all_columns = List.map (fun x -> snd x) (inorder table.columns) in
               List.map
                 (fun col -> get_one_cell_even_length col row longest_length)
                 all_columns)
          ; " |"
          ])
      row_list
  with
  | Not_found -> raise TableDNE
;;

let pretty_print_fields table =
  let longest_length = longest_field_length table in
  let col_num = get_col_num table in
  let row_length = (longest_length * col_num) + (col_num + 7) in
  let row_separator = repeat row_length "-" in
  let pair_list = List.split (get_field_name_list_internal table) in
  let field_list = buff_up_string_list (fst pair_list) longest_length in
  let type_list =
    buff_up_string_list
      (List.map (fun x -> string_of_data_type x) (snd pair_list))
      longest_length
  in
  String.concat
    ""
    [ "| "
    ; String.concat " | " field_list
    ; " |\n"
    ; row_separator
    ; "\n| "
    ; String.concat " | " type_list
    ; " |\n"
    ; row_separator
    ]
;;

let pretty_print table =
  let longest_length = longest_field_length table in
  let col_num = get_col_num table in
  let row_length = (longest_length * col_num) + (col_num + 7) in
  let row_separator = repeat row_length "-" in
  Format.sprintf
    "\n @[Table: %s@] \n %d columns * %d entries\n"
    (get_table_name_internal table)
    (get_col_num table)
    (get_row_num table)
  ^ String.concat
      ""
      [ "\n"
      ; row_separator
      ; "\n"
      ; pretty_print_fields table
      ; "\n"
      ; String.concat
          (String.concat row_separator [ "\n"; "\n" ])
          (get_all_rows_even_length table longest_length)
      ]
;;

let create_empty_column field_name data_type =
  if field_name = "" then raise IllegalName else { field_name; data_type; data = empty }
;;

let create_empty_table table_name =
  if table_name = ""
  then raise IllegalName
  else { table_name; columns = empty; num_rows = 0 }
;;

let create_empty_database database_name =
  if database_name = ""
  then raise IllegalName
  else { database_name; tables = empty; num_tables = 0 }
;;

let insert_column_internal table column =
  { table with
    num_rows = 0
  ; columns = insert (generate_new_key table.columns, column) table.columns
  }
;;

let get_empty_columns field_name_type_alist =
  List.map (fun x -> create_empty_column (fst x) (snd x)) field_name_type_alist
;;

let get_new_table table_name field_name_type_alist =
  let empty_table = create_empty_table table_name in
  let get_empty_columns field_name_type_alist =
    List.map (fun x -> create_empty_column (fst x) (snd x)) field_name_type_alist
  in
  let empty_columns = get_empty_columns field_name_type_alist in
  List.fold_left (fun x y -> insert_column_internal x y) empty_table empty_columns
;;

let create_table db table_name field_name_type_alist =
  if duplicate_in_list (fun x y -> compare (fst x) (fst y)) field_name_type_alist
  then raise IllegalName
  else (
    let new_table = get_new_table table_name field_name_type_alist in
    let new_db =
      { db with
        tables = insert (generate_new_key db.tables, new_table) db.tables
      ; num_tables = db.num_tables + 1
      }
    in
    new_db)
;;

(* return a list of row numbers that needs to be kept in the new table *)
let get_row_numbers_to_keep
  (db : database)
  (filtering_function : string list * string list -> bool)
  (table_name : string)
  : int list
  =
  let table = tree_find (fun table -> table.table_name = table_name) db.tables in
  let row_num_list = get_list_of_row_numbers table in
  let col_names_list = fst (List.split (get_field_name_list_internal table)) in
  List.filter
    (fun row_num ->
      filtering_function (col_names_list, get_one_row db table_name row_num))
    row_num_list
;;

(* return a new column that is the old column whose data tree only
    contain the rows of data that we want to keep *)
let filter_some_row (old_column : column) (rows_to_keep : int list) : column =
  let old_data_tree = get_column_data_internal old_column in
  let new_data_tree =
    filter_based_on_key
      (fun row_num -> List.exists (fun elem -> elem = row_num) rows_to_keep)
      old_data_tree
  in
  let updated_data_tree = update_key new_data_tree in
  { old_column with data = updated_data_tree }
;;

(* return a new table with the function f applied to each column *)
let get_new_table (old_table : table) (f : column -> column) (nr : int) : table =
  let old_column_tree = old_table.columns in
  let new_column_tree = map f old_column_tree in
  { old_table with columns = new_column_tree; num_rows = nr }
;;

let filter_table_rows
  (db : database)
  (table_name : string)
  (filtering_function : string list * string list -> bool)
  : table
  =
  let table = tree_find (fun table -> table.table_name = table_name) db.tables in
  let rows_to_keep = get_row_numbers_to_keep db filtering_function table_name in
  get_new_table
    table
    (fun column -> filter_some_row column rows_to_keep)
    (List.length rows_to_keep)
;;

let negate_filtering_function
  (filtering_function : string list * string list -> bool)
  (pair_list : string list * string list)
  =
  not (filtering_function pair_list)
;;

let get_new_db (db : database) (table_name : string) (new_table : table) =
  let new_db =
    { db with
      tables =
        (let key =
           get_key (fun (table, index) -> table.table_name = table_name) db.tables
         in
         update key new_table db.tables)
    }
  in
  new_db
;;

let delete_row
  (db : database)
  (table_name : string)
  (filtering_function : string list * string list -> bool)
  =
  try
    let old_table = tree_find (fun table -> table.table_name = table_name) db.tables in
    let negated = negate_filtering_function filtering_function in
    let new_table = filter_table_rows db old_table.table_name negated in
    let new_db = get_new_db db table_name new_table in
    new_db
  with
  | Not_found -> raise TableDNE
;;

let drop_table db table_name =
  let new_database =
    { db with
      tables = filter_based_on_value (fun x -> x.table_name <> table_name) db.tables
    ; num_tables = db.num_tables - 1
    }
  in
  if size new_database.tables = size db.tables - 1 then new_database else raise TableDNE
;;

let extract_all_columns (table : table) =
  filter_based_on_value (fun col -> true) table.columns
;;

(* filters selected columns of the table according to a field name list *)
let select_column (table : table) (field_list : string list) : table =
  let new_table =
    let new_cols =
      match field_list with
      | [ "*" ] -> extract_all_columns table
      | _ ->
        filter_based_on_value
          (fun col -> List.exists (fun name -> name = col.field_name) field_list)
          table.columns
    in
    match field_list with
    | [ "*" ] -> { table with columns = new_cols }
    | _ ->
      if List.length field_list = size new_cols
      then { table with columns = new_cols }
      else raise ColumnDNE
  in
  new_table
;;

(* returns a new table with only selected columns and rows *)
let select
  (db : database)
  (table_name : string)
  (field_list : string list)
  (filtering_function : string list * string list -> bool)
  =
  try
    let old_table = tree_find (fun table -> table.table_name = table_name) db.tables in
    let new_table = filter_table_rows db old_table.table_name filtering_function in
    let cols_filtered_table = select_column new_table field_list in
    cols_filtered_table
  with
  | Not_found -> raise TableDNE
;;

let default_of_data_type (data_type : data_type) =
  match data_type with
  | Int -> string_of_int default_int
  | Float -> string_of_float default_float
  | String -> default_string
;;

(* insert the default value into a designated column *)
let insert_default_into_column (old_column : column) : column =
  let data_type = old_column.data_type in
  let old_data = old_column.data in
  let new_row = generate_new_key old_data in
  let new_data = insert (new_row, default_of_data_type data_type) old_data in
  { old_column with data = new_data }
;;

let insert_default_in_every_column (old_table : table) =
  get_new_table (old_table : table) insert_default_into_column (old_table.num_rows + 1)
;;

let update_data_in_column (column : column) (new_data : string) (row_num : int) : column =
  let data = column.data in
  let new_data_tree = update row_num new_data data in
  { column with data = new_data_tree }
;;

let rec update_all_rows (column : column) (new_data : string) (row_num : int list)
  : column
  =
  match row_num with
  | [] -> column
  | h :: t -> update_data_in_column (update_all_rows column new_data t) new_data h
;;

(* update the column at the index in the table to a new column and return the new table *)
let update_column_in_table (col_key : int) (new_column : column) (table : table) =
  let old_column_tree = table.columns in
  let new_column_tree = update col_key new_column old_column_tree in
  { table with columns = new_column_tree }
;;

(* return true if data does not belong to this type *)
let wrong_type data (dp : data_type) =
  let num_int = int_of_string_opt data in
  let num_float = float_of_string_opt data in
  match dp with
  | Int -> num_int = None
  | Float -> num_float = None
  | String -> false
;;

let get_col_key column_name col_tree =
  try get_key_col (fun (col, index) -> col.field_name = column_name) col_tree with
  | NotFound -> raise (Failure "raised at get_key")
;;

let rec update_table_helper lst table col_tree rows_to_keep =
  match lst with
  | [] -> table
  | (column_name, data) :: t ->
    let col_key = get_col_key column_name col_tree in
    let column =
      try get col_key col_tree with
      | NotFound -> raise (Failure "raised at get col_key ...")
    in
    let col_type = column.data_type in
    if wrong_type data col_type
    then raise WrongType
    else (
      let new_column = update_all_rows column data rows_to_keep in
      let t =
        update_column_in_table
          col_key
          new_column
          (update_table_helper t table col_tree rows_to_keep)
      in
      t)
;;

let generate_new_db db table_name new_table =
  { db with
    tables =
      (let key =
         get_key (fun (table, index) -> table.table_name = table_name) db.tables
       in
       update key new_table db.tables)
  }
;;

let get_new_table table_name db fieldname_type_value_list filtering_function =
  let table = tree_find (fun table -> table.table_name = table_name) db.tables in
  let col_tree = table.columns in
  let rows_to_keep = get_row_numbers_to_keep db filtering_function table.table_name in
  update_table_helper fieldname_type_value_list table col_tree rows_to_keep
;;

(* get the row numbers to update, for each column, these row numbers
    need to to be filled with the new value *)
let update_row
  (db : database)
  table_name
  (fieldname_type_value_list : (string * string) list)
  filtering_function
  =
  try
    let new_table =
      get_new_table table_name db fieldname_type_value_list filtering_function
    in
    let new_db = generate_new_db db table_name new_table in
    new_db
  with
  | NotFound -> raise ColumnDNE
  | Not_found -> raise TableDNE
;;

let update_one_row_only
  table
  (fieldname_type_value_list : (string * string) list)
  new_row_index
  =
  let col_tree = table.columns in
  let rows_to_keep = [ new_row_index ] in
  let new_table =
    update_table_helper fieldname_type_value_list table col_tree rows_to_keep
  in
  new_table
;;

let get_final_table table_with_default_inserted fieldname_type_value_list new_row_index =
  let new_table =
    update_one_row_only
      table_with_default_inserted
      fieldname_type_value_list
      new_row_index
  in
  let final_table = { new_table with num_rows = new_table.num_rows } in
  final_table
;;

let insert_aux
  (db : database)
  (table_name : string)
  (fieldname_type_value_list : (string * string) list)
  =
  try
    let key = get_key (fun (table, index) -> table.table_name = table_name) db.tables in
    let table = get key db.tables in
    let new_row_index = get_row_num table in
    let table_with_default_inserted = insert_default_in_every_column table in
    get_final_table table_with_default_inserted fieldname_type_value_list new_row_index
  with
  | NotFound -> raise TableDNE
;;

let get_new_db_insert db table_name final_table =
  { db with
    tables =
      (let key =
         get_key (fun (table, index) -> table.table_name = table_name) db.tables
       in
       update key final_table db.tables)
  }
;;

let insert_row
  (db : database)
  (table_name : string)
  (fieldname_type_value_list : (string * string) list)
  =
  try
    let final_table = insert_aux db table_name fieldname_type_value_list in
    let new_db = get_new_db_insert db table_name final_table in
    new_db
  with
  | NotFound -> raise ColumnDNE
;;
