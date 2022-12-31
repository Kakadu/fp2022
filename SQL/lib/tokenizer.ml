open Type

let to_string_list input =
  input
  |> String.trim
  |> String.split_on_char ' '
  |> List.map String.trim
  |> List.filter (fun e -> e <> "")
;;

let match_terminal s =
  try Terminal (Int (int_of_string s)) with
  | _ ->
    (try Terminal (Float (float_of_string s)) with
     | _ -> Terminal (String s))
;;

let match_token = function
  | "QUIT" -> exit 0
  | "CREATE" -> Command Create
  | "SELECT" -> Command Select
  | "DROP" -> Command Drop
  | "INSERT" -> Command Insert
  | "DELETE" -> Command Delete
  | "UPDATE" -> Command Update
  | "DATABASE" -> Target Database
  | "TABLE" -> Target Table
  | "SET" -> SubCommand Set
  | "VALUES" -> SubCommand Values
  | "INTO" -> SubCommand Into
  | "FROM" -> SubCommand From
  | "WHERE" -> SubCommand Where
  | "=" -> BinaryOp EQ
  | ">" -> BinaryOp GT
  | ">=" -> BinaryOp GE
  | "<" -> BinaryOp LT
  | "<=" -> BinaryOp LE
  | "!=" -> BinaryOp NE
  | "AND" -> LogicOp AND
  | "OR" -> LogicOp OR
  | ";" -> EndOfQuery EOQ
  | "SAVE" -> Command Save
  | "READ" -> Command Read
  | s -> match_terminal s
;;

let tokenize s = List.map (fun e -> match_token e) (to_string_list s)
