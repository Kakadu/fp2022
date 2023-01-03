(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Map
open Base

type state = { local_vars : (string, value, String.comparator_witness) Map.t }

let empty_state : state = { local_vars = Map.empty (module String) }

let get_variable (st : state) (name : string) : value =
  match Map.find st.local_vars name with
  | Some v -> v
  | None -> failwith "Variable does not exist"
;;

let set_local_var (st : state) (name : string) (new_v : value) : state =
  { local_vars = Map.set st.local_vars ~key:name ~data:new_v }
;;
