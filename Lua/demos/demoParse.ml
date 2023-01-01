(** Copyright 2021-2022, Arthur Alekseev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Lua_lib.Parser
open Lua_lib.Ast
include Parser

let print_ast = false

let print_ast code =
  match parse (Parser.string_to_input code) with
  | Failed m | HardFailed m ->
    print_endline (Printf.sprintf "Parser failed with message: %s" m)
  | Parsed (h, t) ->
    (match Parser.input_to_string t with
     | "" -> print_endline (Printf.sprintf "%s" (Ast.show_ast h))
     | t -> print_endline (Printf.sprintf "Parser failed and it is unparsed: %s" t))
;;

module _ = struct
  let () = print_ast (Stdio.In_channel.input_all Caml.stdin)
end
