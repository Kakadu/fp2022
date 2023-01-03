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

let drop_first f = 
  let g _ z = f z in
  g

let std_variables =
  [ "puts", Function ("puts", [ "s" ], drop_first ruby_puts)
  ; "reads", Function ("reads", [], drop_first ruby_reads)
  ; "readi", Function ("readi", [], drop_first ruby_readi)
  ]
;;

let initial_state =
  let step (st : state) (vt : string * value) =
    match vt with
    (** replace with set class var *)
    | name, v -> set_local_var st name v
  in
  List.fold_left step empty_state std_variables
;;
