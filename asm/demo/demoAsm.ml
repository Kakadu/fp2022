open Asm.Interpreter
open Asm.Parser
open Int64

let () =
  let open Interpret (Result) in
  let code = Stdio.In_channel.input_all stdin in
  let env = prep r_list in
  match eval code with
  | Parsed (Ast ast) -> (
      match interpret env [] ast with
      | Ok (env, s, _) ->
          List.iter print_endline (List.map to_string s);
          print_string @@ show_envr env
      | Error msg -> print_string msg)
  | Failed msg -> print_string msg
