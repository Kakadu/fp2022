open Parser
open Interpreter_class.Interpreter_for_classes (Interpreter_class.Result)
open Interpreter.Interpreter (Interpreter_class.Result)
open Tables

let print_ctx_res res_context = function
  | true -> print_endline (show_context res_context ^ "\n")
  | false -> print_endline ""

let test_interpret class_list_ast class_map tf =
  match interpret_classes class_list_ast class_map with
  | Error m -> print_endline m
  | Ok load_map -> (
    match start_interpreting load_map with
    | Error m -> print_endline m
    | Ok res_context -> print_ctx_res res_context tf )

let interpret s tf =
  let parse_s = Option.get (apply_parser parser s) in
  test_interpret parse_s KeyMap.empty tf
