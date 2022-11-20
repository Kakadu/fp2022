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

(* Return true if a target is present somewhere in targets in rules *)
let rules_has_target target rules =
  Printf.printf "Checking %s\n" target;
  let rule_has_target rule =
    Option.is_some (List.find_opt (String.equal target) rule.targets)
  in
  let rules_with_target = List.filter rule_has_target rules in
  Printf.printf "ans is %b\n" (List.length rules_with_target = 0);
  List.length rules_with_target <> 0
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
  ; dfs_recompile : bool NodeMap.t (* Should we recompile this target? *)
  ; rules : rule_as_lists list
  ; graph : graph
  }

let dfs_init_info rules graph =
  let edgeList node =
    match Hashtbl.find_opt graph node with
    | None -> []
    | Some data -> data.deps
  in
  { dfs_dtime = NodeMap.empty
  ; dfs_ftime = NodeMap.empty
  ; dfs_parents = NodeMap.empty
  ; dfs_time = 0
  ; dfs_edgeList = edgeList (*(fun node -> .deps)*)
  ; dfs_recompile = NodeMap.empty
  ; rules
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

(* increment time, set discover time for node, mark node Gray, set bit to false *)
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

(* set flag for node *)
let dfs_set_flag info node flag =
  let recompile = NodeMap.add node flag info.dfs_recompile in
  { info with dfs_recompile = recompile }
;;

let flag_of_node info node =
  match NodeMap.find_opt node info.dfs_recompile with
  | None -> false
  | Some flag -> flag
;;

(* Updates info in a way that node y is no longer a neighbour of node x *)
let drop_dependency info x y =
  let new_dfs_edgeList node =
    if node = x
    then List.filter (y |> String.equal |> Fun.negate) (info.dfs_edgeList node)
    else info.dfs_edgeList node
  in
  { info with dfs_edgeList = new_dfs_edgeList }
;;

(* set recompile flag for all parents of target (but not for the target itself) *)
let set_recompile_flag_for_all_parents info target =
  let rec set_flag info t =
    match NodeMap.find_opt t info.dfs_parents with
    | None -> info
    | Some parent -> set_flag (dfs_set_flag info parent true) parent
  in
  set_flag info target
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

(*
                    Algorithm to decide whether we should recompile target X

                                   ┌────────────────────┐  no
                                   │Should recompile flg├───────┐
                                   └───────┬────────────┘       │
                                        yes│                    │
                                           │                    │
                                      ┌────▼─────┐       yes ┌──▼──────┐   no
                      ┌───────────────┤ recompile│      ┌────┤X exists?├──────┐
                      │               └──────────┘      │    └─────────┘      │
               ┌──────▼──────┐   no                     │                     │
               │ No recipes? ├───────────►end      ┌────▼─────────────┐   ┌───▼────────┐ yes
               └─────┬───────┘                     │get timestamp     │   │Rule in Ast?├────┐
                     │                             │of X and parent(X)│   └─────┬──────┘    │
       ┌─────────┐   │                             └────┬─────────────┘      no │           │
   yes │X is main◄───┘                                  │               ┌───────▼─┐     ┌───▼─────┐
     ┌─┤ target? │                     ┌──────────────┐ │               │X is main│     │recompile│
     │ └──┬──────┘                     │X > parent(X)?├─┘               │ target? │     └─────┬───┘
     │    │no                          └─┬──────────┬─┘                 └┬───────┬┘           │
     │    └──────────►end             yes│          │ no              yes│       │no          │
     │                                   │          │                    │       ▼            │
     │                                   │          └─────────► end      │    raise NoRule    │
     │                     ┌─────────────▼────────┐                      │                    │
┌────▼────────────────┐    │Set recompile flag for│                      │            ┌───────▼─────┐
│raise NothingToBeDone│    │   all parents of X   │                      │            │ No recipes? │
└─────────────────────┘    └────────────┬─────────┘                      │            └───────┬─────┘
                                        │                                │                    │
                                        │                                │              ┌─────▼───┐
                                        ▼                                │          yes │X is main│  no
                                       end                               ├──────────────┤ target? ├────────►end
                                                                         │              └─────────┘
                                                                ┌────────▼────────────┐
                                                                │raise NothingToBeDone│
                                                                └─────────────────────┘


One small fix, cause main target doesn't have a parent:

                      ┌─────────┐          yes ┌─────────┐
 ┌──────────────┐ yes │X is main◄──────────────┤X exists?│
 │raise UpToDate◄─────┤ target? │              └─────────┘
 └──────────────┘     └──┬──────┘
                         │no        ┌──────────────────┐
                         └─────────►┘get timestamp     │

 *)

exception Recipe of string * int
exception NoRule of string * string
exception UpToDate of string
exception NothingToBeDone of string

let get_recipes info target =
  match Hashtbl.find_opt info.graph target with
  | None -> []
  | Some data -> data.recipes
;;

let recompile info target is_main_target =
  let recipes = get_recipes info target in
  let execute_recipes =
    let rec traverse_recipes = function
      | [] -> info
      | recipe :: tl ->
        print_endline recipe;
        let rc = Sys.command recipe in
        if rc <> 0 then raise (Recipe (target, rc)) else traverse_recipes tl
    in
    traverse_recipes recipes
  in
  if List.length recipes = 0
  then if is_main_target then raise (NothingToBeDone target) else info
  else execute_recipes
;;

(* Could raise exception *)
let try_execute_recipes info target is_main_target =
  if flag_of_node info target
  then recompile info target is_main_target
  else if Sys.file_exists target
  then
    if is_main_target
    then raise (UpToDate target)
    else (
      let parent = NodeMap.find target info.dfs_parents in
      if Sys.file_exists parent = false
      then set_recompile_flag_for_all_parents info target
      else (
        let parent_time = (Unix.stat parent).st_mtime in
        let target_time = (Unix.stat target).st_mtime in
        if target_time > parent_time
        then set_recompile_flag_for_all_parents info target
        else info))
  else if rules_has_target target info.rules
  then recompile info target is_main_target
  else if is_main_target
  then raise (NothingToBeDone target)
  else raise (NoRule (target, NodeMap.find target info.dfs_parents))
;;

(* Traverse graph from node u, executing recipes of u in the end *)
(* Could raise exception *)
let rec dfs_visit info u =
  (* Visit all neighbours v of node u *)
  let visit_node info v =
    let info = dfs_add_parent info v u in
    if dfs_color info v = Gray (* on recursion stack -> cycle found *)
    then (
      print_cycle info.dfs_parents v;
      drop_dependency info u v)
    else (
      let info = dfs_discover info v in
      (* recurse on v *)
      let info = dfs_visit info v in
      let info = dfs_finish info v in
      try_execute_recipes info v false)
  in
  List.fold_left visit_node info (info.dfs_edgeList u)
;;

(* Could raise exception *)
let dfs rules graph target =
  let info = dfs_init_info rules graph in
  let info = dfs_discover info target in
  let info = dfs_visit info target in
  try_execute_recipes info target true
;;

(* Could raise exception *)
let build_target rules target =
  let graph = graph_of_rules rules in
  dfs rules graph target
;;

let interpret (ast : ast) targets =
  let rules = rules_of_ast ast in
  let rules = rules_as_lists_of_rules rules in
  let rec traverse_targets = function
    | [] -> Ok ""
    | target :: tl ->
      (try
         let _ = build_target rules target in
         traverse_targets tl
       with
       | Recipe (target, rc) ->
         Error (Printf.sprintf "make: *** [Makefile: '%s'] Error %d" target rc)
       | NoRule (target, main_target) ->
         Error
           (Printf.sprintf
              "make: *** No rule to make target '%s', needed by %s.  Stop."
              target
              main_target)
       | UpToDate target -> Ok (Printf.sprintf "make: '%s' is up to date." target)
       | NothingToBeDone target ->
         Ok (Printf.sprintf "make: Nothing to be done for '%s'." target))
  in
  match targets with
  (* When no targets are specified, choose first one *)
  | [] -> traverse_targets [ rules |> get_unique_filenames |> List.hd ]
  | _ -> traverse_targets targets
;;
