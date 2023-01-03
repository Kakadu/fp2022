(** Copyright 2021-2022, ol-imorozko and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Graphlib.Std
open Ppx_hash_lib.Std.Hash.Builtin
open Ppx_compare_lib.Builtin
open Ppx_sexp_conv_lib.Conv

(** Extracts list of rules from ast. Keeps rules order *)
let rules_of_ast : ast -> rule list =
  let rule_of_expr = function
    | Rule rule -> Some rule
    (* For now expr could only be a Rule, so we don't need line below *)
    (* | _ -> None *)
  in
  function
  | rule, [] -> [ rule ]
  | rule, exprs -> rule :: List.filter_map rule_of_expr exprs
;;

(** Original rule type has tuple as "targets" type,
But it is easier to works with lists *)
type rule_as_lists =
  { targets : string list
  ; prerequisites : string list
  ; recipes : recipe list
  }

(** Transform list of ast's rules to rule_as_lists list
 Keeps rules order *)
let rules_as_lists_of_rules =
  List.map (fun ({ targets = x, l; prerequisites; recipes } : rule) ->
    { targets = x :: l; prerequisites; recipes })
;;

(* Return true if a target is present somewhere in targets in one rule *)
let rule_has_target target rule = List.mem target rule.targets

(* Return true if a target is present somewhere in targets in list of rules *)
let rules_has_target target rules =
  Option.is_some (List.find_opt (rule_has_target target) rules)
;;

(** Return recipes for specified target among all rules.
If two or more rules are found with the same target in the target list,
and with different recipes, the newer one will be used *)
let recipes_of_target target rules =
  let rules_with_target = List.filter (rule_has_target target) rules in
  let recipes_list =
    List.map (fun (rule : rule_as_lists) -> rule.recipes) rules_with_target
  in
  let rec traverse_recipes = function
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
  traverse_recipes recipes_list
;;

(** Return dependencies for specified target among all rules. *)
let dependencies_of_target target rules =
  let rules_with_target = List.filter (rule_has_target target) rules in
  let dependencies_list =
    List.map (fun (rule : rule_as_lists) -> rule.prerequisites) rules_with_target
  in
  List.concat dependencies_list |> Core.List.stable_dedup
;;

(* Get unique filenames occuring in targets and dependencies among all rules *)
let get_unique_filenames rules =
  let rec get_all_filenames = function
    | [] -> []
    | { targets = t; prerequisites = p; recipes = _ } :: tl ->
      t @ p @ get_all_filenames tl
  in
  get_all_filenames rules |> Core.List.stable_dedup
;;

(*========GRAPH STUFF========*)

(* This is mutable data structure.
   We will use this type in Graphlib.Labeled, which requires "Node" type
   to be Regular.Std.Opaque.S, which means it should include Core_kernel.Comparable.S
   and Core_kernel.Hashable.S. *)
module Node : sig
  type t =
    { target : string
    ; recipes : recipe list [@compare.ignore] [@hash.ignore] [@sexp.ignore]
    }
  [@@deriving compare, hash, sexp]

  include Core.Comparable.S with type t := t
  include Core.Hashable.S with type t := t
end = struct
  module T = struct
    type t =
      { target : string
      ; recipes : recipe list [@compare.ignore] [@hash.ignore] [@sexp.ignore]
      }
    [@@deriving compare, hash, sexp]
  end

  include T
  include Core.Comparable.Make (T)
  include Core.Hashable.Make (T)
end

(* TODO: Use label (Unit) somehow. Probably mark PHONY targets with it *)
module G = Graphlib.Labeled (Node) (Unit) (Unit)

module VertexMap = Map.Make (struct
  type t = string [@@deriving compare]
end)

(* Map to get corresponding vertex in graph by label (string),
   cause Graphlib do not provide such interface. *)
type vertex_of_target = G.Node.t VertexMap.t

(* Creates new vertex and puts in into the map.
   If it's already present, simply returns that vertex *)
let create_vertex map target rules =
  match VertexMap.find_opt target map with
  | None ->
    let vertex =
      G.Node.create
        { node = { target; recipes = recipes_of_target target rules }; node_label = () }
    in
    let map = VertexMap.add target vertex map in
    vertex, map
  | Some vertex -> vertex, map
;;

(* Constructs graph and map to get corresponding vertex by target's name *)
let graph_of_rules rules =
  let map : vertex_of_target = VertexMap.empty in
  let g = G.empty in
  let filenames = get_unique_filenames rules in
  let rec fill_graph map g = function
    | [] -> g, map
    | target :: tl ->
      let parent, map = create_vertex map target rules in
      let g = G.Node.insert parent g in
      let deps = dependencies_of_target target rules in
      let rec fill_children_edges map g = function
        | [] -> g, map
        | target :: tl ->
          let child, map = create_vertex map target rules in
          let g = G.Edge.insert (G.Edge.create parent child ()) g in
          fill_children_edges map g tl
      in
      let g, map = fill_children_edges map g deps in
      fill_graph map g tl
  in
  fill_graph map g filenames
;;

(* Traverse @targets, addding corresponding nodes to graph if they do not exist *)
let add_default_goals targets map rules g =
  let rec try_add_targets map rules g = function
    | [] -> g, map
    | target :: tl ->
      let g, map =
        match VertexMap.find_opt target map with
        | None ->
          let vertex, map = create_vertex map target rules in
          G.Node.insert vertex g, VertexMap.add target vertex map
        | Some _ -> g, map
      in
      try_add_targets map rules g tl
  in
  try_add_targets map rules g targets
;;

exception Recipe of string * int
exception NoRule of string
exception UpToDate of string
exception NothingToBeDone of string

(* Set of nodes that require recompiling *)
type dfs_state =
  { nodes_to_recompile : G.Node.Set.t
  ; finish_component : bool
  }

(* fold on @n predecessors in graph @g. Loops are ignored *)
let fold_pred f g n init =
  let preds = G.Node.preds n g |> Base.Sequence.to_list_rev in
  List.fold_left (fun a x -> if n = x then a else f x a) init preds
;;

let recompile (node : G.Node.t) is_default_goal =
  let recipes = node.node.recipes in
  let target = node.node.target in
  let execute_recipes =
    let rec traverse_recipes = function
      | [] -> ()
      | recipe :: tl ->
        let cmd =
          match recipe with
          | Echo x ->
            print_endline x;
            x
          | Silent x -> x
        in
        let rc = Sys.command cmd in
        if rc <> 0 then raise (Recipe (target, rc)) else traverse_recipes tl
    in
    traverse_recipes recipes
  in
  match recipes with
  | [] -> if is_default_goal then raise (NothingToBeDone target)
  | _ -> execute_recipes
;;

(* Returns true if for at least one parent of X
   timestamp (X) > timestamp (Parent), or Parent does not exist  *)
let check_timestamps_of_parents graph node target =
  let check_timestamps target parent =
    (not (Sys.file_exists parent))
    ||
    let parent_time = (Unix.stat parent).st_mtime in
    let target_time = (Unix.stat target).st_mtime in
    target_time > parent_time
  in
  fold_pred
    (fun parent (ans, target) ->
      let ans = ans || check_timestamps target parent.node.target in
      ans, target)
    graph
    node
    (false, target)
  |> fst
;;

let rec mark_all_parents_as_recompile graph node nodes_to_recompile =
  let nodes_to_recompile =
    fold_pred
      (fun parent set ->
        let set = mark_all_parents_as_recompile graph parent set in
        G.Node.Set.add set node)
      graph
      node
      nodes_to_recompile
  in
  G.Node.Set.add nodes_to_recompile node
;;

(*
  Algorithm to decide whether we should recompile target X

                     ┌────────────────┐  no
        ┌────────────┤Should recompile├───────┐
        │            └────────────────┘       │
     yes│                                     │
        │                                     │
   ┌────▼─────┐                        yes ┌──▼──────┐   no
   │ recompile│               ┌────────────┤X exists?├──────┐
   └──────────┘               │            └─────────┘      │
                      ┌───────▼─┐                           │
               yes    │X is def │                       ┌───▼────────┐ yes
         ┌────────────┤  goal?  │                       │Rule in Ast?├────┐
         ▼            └────────┬┘                       └─────┬──────┘    │
   raise UpToDate              │no                         no │           │
                               │                      ┌───────▼─┐     ┌───▼─────┐
                          ┌────▼───────────┐          │X is def │     │recompile│
                          │Iterate over all│          │  goal?  │     └─────────┘
                          │parents of X    │          └┬───────┬┘
                          └────┬───────────┘        yes│       │ no
                               │                       │       ▼
                        yes ┌──▼──────┐   no           │    raise NoRule
                       ┌────┤P exists?├───┐            │
                       │    └─────────┘   │            │
                       │                  │            ▼
               ┌───────┴─────────┐        │    raise NothingToBeDone
               │time(X) > time(P)│        │
               └───────┬─────────┘        │
                    yes│                  │
                       │                  │
                       │                  │
                   ┌───▼──────────────────▼────┐
                   │mark ALL parents of X      │
                   │recursively as recompile   │
                   └───────────────────────────┘
 *)
let try_execute_recipes (node : G.Node.t) graph rules nodes_to_recompile is_default_goal =
  let target = node.node.target in
  if G.Node.Set.mem nodes_to_recompile node
  then (
    recompile node is_default_goal;
    nodes_to_recompile)
  else if Sys.file_exists target
  then
    if is_default_goal
    then raise (UpToDate target)
    else if check_timestamps_of_parents graph node target
    then mark_all_parents_as_recompile graph node nodes_to_recompile
    else nodes_to_recompile
  else if rules_has_target target rules
  then (
    recompile node is_default_goal;
    nodes_to_recompile)
  else if is_default_goal
  then raise (NothingToBeDone target)
  else raise (NoRule target)
;;

(* Traversing **only one** component starting from @node in a
   depth-first-search manner, applying f when leaving node.
   Loops are ignored and info about them are printed to stdout. *)
let traverse_component node graph f =
  let reachable =
    Graphlib.fold_reachable (module G) graph node ~init:G.Node.Set.empty ~f:G.Node.Set.add
  in
  Graphlib.depth_first_search
    (module G)
    graph
    ~start:node
    ~start_tree:(fun node state ->
      if G.Node.Set.mem reachable node
      then state
      else { state with finish_component = true })
    ~init:{ nodes_to_recompile = G.Node.Set.empty; finish_component = false }
    ~leave_edge:(fun kind edge state ->
      match kind with
      | `Back ->
        Printf.printf
          "make: Circular %s <- %s dependency dropped.\n"
          (G.Edge.src edge).node.target
          (G.Edge.dst edge).node.target;
        state
      | _ -> state)
    ~leave_node:f
;;

(* Could raise exception *)
let build_target target map graph rules =
  (* This will find a node cause all possible targets should be
  in the graph prior to calling the "build_target" function.*)
  let node = VertexMap.find target map in
  traverse_component node graph (fun _ node state ->
    if state.finish_component
    then state
    else
      { state with
        nodes_to_recompile =
          try_execute_recipes
            node
            graph
            rules
            state.nodes_to_recompile
            (target = node.node.target)
      })
;;

let interpret ast targets =
  let rules = ast |> rules_of_ast |> rules_as_lists_of_rules in
  let graph, map = graph_of_rules rules in
  let graph, map = add_default_goals targets map rules graph in
  let rec traverse_targets = function
    | [] -> Ok ""
    | target :: tl ->
      (try
         let _ = build_target target map graph rules in
         traverse_targets tl
       with
       | Recipe (target, rc) ->
         Error (Printf.sprintf "make: *** [Makefile: '%s'] Error %d" target rc)
       | NoRule target ->
         Error (Printf.sprintf "make: *** No rule to make target '%s'. Stop." target)
       | UpToDate target -> Ok (Printf.sprintf "make: '%s' is up to date." target)
       | NothingToBeDone target ->
         Ok (Printf.sprintf "make: Nothing to be done for '%s'." target))
  in
  match targets with
  (* When no targets are specified, choose first one *)
  | [] -> traverse_targets [ rules |> get_unique_filenames |> List.hd ]
  | _ -> traverse_targets targets
;;

(* Usage: *)
(* let file = open_out_bin "mygraph.dot" in *)
(* Dot.output_graph file graph; *)
module Dot = Graph.Graphviz.Dot (struct
  module G = Graphlib.To_ocamlgraph (G)
  include G

  let edge_attributes _ = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None

  let vertex_attributes (_ : (Node.t, unit) labeled) =
    (* [ `Shape `Box; (if v.node_label then `Color 16711680 else `Color 0) ] *)
    [ `Shape `Box ]
  ;;

  let vertex_name (v : (Node.t, unit) labeled) = Printf.sprintf "\"%S\"" v.node.target
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)
