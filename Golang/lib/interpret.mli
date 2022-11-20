type interpret_result =
  | Ok
  | FileNotFound
  | CE of string

val interpret : string -> interpret_result
