(** Copyright 2021-2022, Arthur Alekseev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Lua_lib.Parser
open Lua_lib.Interpreter
open Lua_lib.Ast
include Parser

let print_ast = false

let run_from_string code ctx =
  match parse (Parser.string_to_input code) with
  | Failed m | HardFailed m ->
    print_endline (Printf.sprintf "Parser failed with message: %s" m)
  | Parsed (h, t) ->
    (match Parser.input_to_string t with
     | "" ->
       if print_ast
       then print_endline (Printf.sprintf "Parsed ast for input: %s" (Ast.show_ast h))
       else ();
       (match Interpreter.interpret h ctx with
        | Interpreter.Done _ -> ()
        | Fail m -> print_endline (Printf.sprintf "Interpreter failed with message: %s" m))
     | t -> print_endline (Printf.sprintf "Parser failed and it is unparsed: %s" t))
;;

module _ = struct
  let ectx = Interpreter.emptyctx
  let () = run_from_string (Stdio.In_channel.input_all Caml.stdin) ectx
end
