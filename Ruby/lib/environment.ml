(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base

let empty_class_state : class_state = Map.empty (module String)

let set_in_class_state (st : class_state) (name : string) (new_v : value) : class_state =
  Map.set st ~key:name ~data:new_v
;;

let empty_state : state =
  { local_vars = Map.empty (module String)
  ; class_scopes = [ ref (Map.empty (module String)) ]
  }
;;

let from_global (st : state) : state =
  { local_vars = empty_state.local_vars; class_scopes = st.class_scopes }
;;

let set_local_var (st : state) (name : string) (new_v : value) : state =
  { local_vars = Map.set st.local_vars ~key:name ~data:new_v
  ; class_scopes = st.class_scopes
  }
;;

let add_class_scope (st : state) (init_state : class_state ref) : state =
  { local_vars = st.local_vars; class_scopes = [ init_state ] @ st.class_scopes }
;;

let get_class_var (st : state) (name : string) : value =
  let rec get_from_map_stack = function
    | [] -> failwith "Variable does not exist"
    | m :: tail ->
      (match Map.find !m name with
       | Some v -> v
       | None -> get_from_map_stack tail)
  in
  get_from_map_stack st.class_scopes
;;

let get_variable (st : state) (name : string) : value =
  match Map.find st.local_vars name with
  | Some v -> v
  | None -> get_class_var st name
;;

let get_from_class_state (cls_state : class_state) (name : string) : value =
  get_variable (add_class_scope empty_state (ref cls_state)) name
;;

let set_class_var (st : state) (name : string) (new_v : value) =
  match List.hd st.class_scopes with
  | Some cur_class ->
    cur_class := Map.set !cur_class ~key:name ~data:new_v;
    { local_vars = st.local_vars; class_scopes = st.class_scopes }
  | None -> failwith "Class scopes are empty"
;;
