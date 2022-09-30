open Python_lib.Interpreter
open Eval (Result)

let factorial = "x = 0\nwhile x < 10:\n\tx = x + 1\nx"

let test =
  match parse_and_interpet factorial with
  | Ok x -> print_endline x
  | Error x -> print_endline x
;;
