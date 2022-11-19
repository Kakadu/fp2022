(** Copyright 2021-2022, ol-imorozko and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

(* Extracts list of rules from ast *)
(* Keeps rules order *)
let rules_of_ast =
  let rec rules_of_exprs = function
    | [] -> []
    | expr :: tl ->
      (match expr with
       (* For now expr could only be a Rule, so we don't need *)
       (* _ -> rules_of_exprs tl branch below *)
       | Rule rule -> rule :: rules_of_exprs tl)
  in
  function
  | rule, [] -> [ rule ]
  | rule, exprs -> rule :: rules_of_exprs exprs
;;

(* Original rule type has tuple as "targets" type, *)
(* But it is easier to works with lists *)
type rule_as_lists =
  { targets : string list
  ; prerequisites : string list
  ; recipes : string list
  }

(* Transform list of ast's rules to rule_as_lists list*)
(* Keeps rules order *)
let rules_as_lists_of_rules =
  let rule_as_lists_of_rule : rule -> rule_as_lists = function
    | { targets = t; prerequisites = p; recipes = r } ->
      (match t with
       | single_target, target_list ->
         { targets = single_target :: target_list; prerequisites = p; recipes = r })
  in
  List.map rule_as_lists_of_rule
;;

type target_data =
  { deps : string list
  ; recipes : string list
  }

(* Remove duplicates from the list l *)
let remove_duplicates l =
  let cons_uniq l x = if List.mem x l then l else x :: l in
  List.rev (List.fold_left cons_uniq [] l)
;;

(* Return target data among all rules for specified target. *)
(* If two or more rules are found with the same target in the target list, *)
(* and with different recipes, the newer one will be used *)
(*TODO: check that this function actually returns the oldest recipe for the target *)
let target_data_of_rules target rules =
  let rule_has_target rule =
    Option.is_some (List.find_opt (String.equal target) rule.targets)
  in
  let rules_with_target = List.filter rule_has_target rules in
  let list_of_deps_and_recipes =
    List.map (fun rule -> rule.prerequisites, rule.recipes) rules_with_target
  in
  let list_of_deps, list_of_recipes =
    let d = List.map fst list_of_deps_and_recipes in
    let r = List.map snd list_of_deps_and_recipes in
    d, r
  in
  { deps = List.flatten list_of_deps |> remove_duplicates
  ; recipes =
      (let rec traverse_recipes = function
         | [] -> []
         | [ x ] -> x
         | _ :: tl ->
           Printf.eprintf
             "Makefile: warning: overriding recipe for target '%s'\n\
              Makefile: warning: ignoring old recipe for target '%s'\n"
             target
             target;
           traverse_recipes tl
       in
       traverse_recipes list_of_recipes)
  }
;;

(* Get unique filenames occuring in all rules targets and dependencies *)
let get_unique_filenames rules =
  let rec get_all_filenames = function
    | [] -> []
    | { targets = t; prerequisites = p; recipes = _ } :: tl ->
      t @ p @ get_all_filenames tl
  in
  get_all_filenames rules |> remove_duplicates
;;

(* Targets and dependencies graph as adjacency list *)
type graph = (string, target_data) Hashtbl.t

(* Construct graph *)
let graph_of_rules rules =
  let filenames = get_unique_filenames rules in
  let graph : graph = Hashtbl.create (List.length filenames) in
  let rec fill_hashmap = function
    | [] -> graph
    | target :: tl ->
      Hashtbl.add graph target (target_data_of_rules target rules);
      fill_hashmap tl
  in
  fill_hashmap filenames
;;

(* ============DFS TRAVERSAL============ *)

module NodeMap = Map.Make (struct
  type t = string

  let compare : string -> string -> int = compare
end)

type dfs_info =
  { dfs_dtime : int NodeMap.t (* discovery time *)
  ; dfs_ftime : int NodeMap.t (* finishing time *)
  ; dfs_parents : string NodeMap.t (* parent map *)
  ; dfs_time : int (* most recent time *)
  ; dfs_edgeList : string -> string list
  ; graph : graph
  }

let dfs_init_info graph =
  { dfs_dtime = NodeMap.empty
  ; dfs_ftime = NodeMap.empty
  ; dfs_parents = NodeMap.empty
  ; dfs_time = 0
  ; dfs_edgeList = (fun node -> (Hashtbl.find graph node).deps)
  ; graph
  }
;;

type dfs_color_type =
  | White (* not yet discovered *)
  | Gray (* on recursion stack *)
  | Black (* already visited *)

(* DFS helpers *)
let dfs_color info node =
  if not (NodeMap.mem node info.dfs_dtime)
  then White
  else if not (NodeMap.mem node info.dfs_ftime)
  then Gray
  else Black
;;

(* increment time *)
let dfs_inc_time info = { info with dfs_time = info.dfs_time + 1 }

(* increment time, set discover time for node, mark node Gray *)
let dfs_discover info node =
  let info = dfs_inc_time info in
  let dtime = NodeMap.add node info.dfs_time info.dfs_dtime in
  { info with dfs_dtime = dtime }
;;

(* increment time, set finishing time for node, mark node Black *)
let dfs_finish info node =
  let info = dfs_inc_time info in
  let ftime = NodeMap.add node info.dfs_time info.dfs_ftime in
  { info with dfs_ftime = ftime }
;;

(* add parent mapping *)
let dfs_add_parent info node parent =
  let parents = NodeMap.add node parent info.dfs_parents in
  { info with dfs_parents = parents }
;;

(* Updates info in a way that node y is no longer a neighbour of node x *)
let drop_dependency info x y =
  let new_dfs_edgeList node =
    if node = x
    then List.filter (y |> String.equal |> Fun.negate) (info.dfs_edgeList node)
    else info.dfs_edgeList node
  in
  { dfs_dtime = info.dfs_dtime
  ; dfs_ftime = info.dfs_ftime
  ; dfs_parents = info.dfs_parents
  ; dfs_time = info.dfs_time
  ; dfs_edgeList = new_dfs_edgeList
  ; graph = info.graph
  }
;;

let print_cycle parents node =
  print_string "make: Circular ";
  let rec print cur =
    let parent = NodeMap.find cur parents in
    let () = if parent <> node then print parent else print_string parent in
    print_string (" <- " ^ cur)
  in
  print node;
  print_endline " dependency dropped."
;;

exception Recipe of string * int

(* Could raise exception *)
let execute_recipes graph target =
  let rec traverse_recipes = function
    | [] -> ()
    | recipe :: tl ->
      let rc = Sys.command recipe in
      if rc <> 0 then raise (Recipe (target, rc)) else traverse_recipes tl
  in
  traverse_recipes (Hashtbl.find graph target).recipes
;;

(* Traverse graph from node u, executing recipes of u in the end *)
(* Could raise exception *)
let rec dfs_visit info u =
  Printf.printf "running dfs from node %s\n" u;
  (* Visit all neighbours v of node u *)
  let visit_node info v =
    Printf.printf "Looking at %s -- neighbour of %s\n" v u;
    if dfs_color info v = Black
    then (
      Printf.printf "%s is black, skipping\n" v;
      info)
    else (
      let info = dfs_add_parent info v u in
      if dfs_color info v = Gray (* on recursion stack -> cycle found *)
      then (
        Printf.printf "%s is gray, cycle found.\n" v;
        print_cycle info.dfs_parents v;
        drop_dependency info u v)
      else (
        Printf.printf "%s is white, going in it\n" v;
        let info = dfs_discover info v in
        (* recurse on v *)
        let info = dfs_visit info v in
        let info = dfs_finish info v in
        Printf.printf "%s traversal finished\n" v;
        execute_recipes info.graph v;
        info))
  in
  List.fold_left visit_node info (info.dfs_edgeList u)
;;

(* Could raise exception *)
let dfs graph target =
  let info = dfs_init_info graph in
  let info = dfs_discover info target in
  let _ = dfs_visit info target in
  execute_recipes graph target
;;

(* Could raise exception *)
let build_target rules target =
  let graph = graph_of_rules rules in
  dfs graph target
;;

let interpret (ast : ast) targets =
  let rules = rules_of_ast ast in
  let rules = rules_as_lists_of_rules rules in
  let rec traverse_targets = function
    | [] -> Ok ""
    | target :: tl ->
      (try
         build_target rules target;
         traverse_targets tl
       with
       | Recipe (target, rc) ->
         Error (Printf.sprintf "make: *** [Makefile: %s] Error %d" target rc))
  in
  match targets with
  (* When no targets are specified, choose first one *)
  | [] -> traverse_targets [ rules |> get_unique_filenames |> List.hd ]
  | _ -> traverse_targets targets
;;

let rules =
  [ { targets = [ "b"; "c" ]; prerequisites = [ "d"; "e" ]; recipes = [ "echo a" ] }
  ; { targets = [ "a" ]; prerequisites = [ "e" ]; recipes = [ "echo e" ] }
  ; { targets = [ "d"; "x" ]; prerequisites = [ "b"; "i"; "k" ]; recipes = [ "echo d" ] }
  ; { targets = [ "e" ]; prerequisites = [ "k" ]; recipes = [ "echo e" ] }
  ; { targets = [ "e" ]; prerequisites = [ "i" ]; recipes = [ "echo a" ] }
  ]
;;

let%test _ =
  let _ =
    try build_target rules "b" with
    | Recipe (target, rc) -> Printf.printf "make: *** [Makefile: %s] Error %d" target rc
  in
  false
;;
