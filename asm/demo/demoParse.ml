open Asm.Parser
open Asm.Ast

let () =
  let code = Stdio.In_channel.input_all stdin in
  match parse code with
  | Ok ast -> print_endline @@ show_ast ast
  | Error msg -> print_endline msg
