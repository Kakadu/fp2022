(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Environment
open Utils

let ruby_puts = function
  | v :: [] ->
    let () = print_endline (string_of_value v) in
    Nil
  | _ -> failwith "Wrong number of arguments"
;;

let ruby_readi = function
  | [] -> Integer (read_int ())
  | _ -> failwith "readi function doesn't take arguments"
;;

let ruby_reads = function
  | [] -> String (read_line ())
  | _ -> failwith "reads function doesn't take argument"
;;

let std_variables =
  [ "puts", Function ("puts", [ "s" ], ruby_puts)
  ; "reads", Function ("reads", [], ruby_reads)
  ; "readi", Function ("readi", [], ruby_readi)
  ]
;;

let initial_state =
  let step (st : State.storage) (vt : string * value) =
    match vt with
    | name, v -> State.set_variable st name v
  in
  List.fold_left step State.create std_variables
;;
