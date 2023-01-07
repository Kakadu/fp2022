(** Copyright 2021-2022, ol-imorozko and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Graphlib.Std
open Ppx_hash_lib.Std.Hash.Builtin
open Ppx_compare_lib.Builtin
open Ppx_sexp_conv_lib.Conv

(** The context needed to properly expand the recipe *)
type recipe_context =
  { current_target : string
  ; first_prerequisite : string
  ; stem : string
  }
[@@deriving compare, sexp, hash, show { with_path = false }]

let default_recipe_context = { current_target = ""; first_prerequisite = ""; stem = "" }

(** Processed rules with substitution performed.

    Variables in all parts of a makefile are expanded when read,
    except for in recipes and the right-hand sides of variable definitions *)
type processed_rule =
  { targets : string list
  ; prerequisites : string list
  ; recipes : recipe list * recipe_context
  }
[@@deriving show { with_path = false }]

(** Value of a variable.

    It's easier to store it as a string list in case of substitution in
    recipes/prerequisites, which are a string list.recipes
    When substituted in recipe, we'll flatten it. *)
type var_value =
  | Expanded of string list
  | Raw of word list
[@@deriving compare, show { with_path = false }]

module VarMap = Map.Make (struct
  type t = string [@@deriving compare]
end)

(** Map to get var_value by it's name (string) *)
type varmap = var_value VarMap.t

(** Transform ast to the list of exprs.

    Ast has type rule * expr list cause we want to make sure that Makefile
    contains at least one rule. However, we need to put it back in order to
    properly restore it's position, cause it's important due to the immediate
    expansion of variables in the targets/prerequisites *)
let exprs_of_ast ((rule, pos), exprs) =
  let insert_at_pos pos x l =
    match Core.List.split_n l pos with
    | l1, l2 -> l1 @ [ Rule x ] @ l2
  in
  insert_at_pos pos rule exprs
;;

(** Expands variables in a word, turning it into a string list.

    Again, is's easier to think of word as a list of strings, separated by
    a spaces in case of substitution into targets/prerequisites. If we're in
    a recipe parsing context, we could always flatten the list.

    An example of expanding "a$(a)123$(x)", where "a = x y" and "x = p q":
    a$(a)123$(x) -> a[x; y]123[p; q] -> [ax; y]123[p; q] -> [ax; y123][p; q] -> [ax; y123p; q]
*)
let rec process_word
  ?(recipe_context = Option.None)
  ?(prerequisite_context = Option.None)
  map
  word
  =
  (* concat_to_last_element ["a"; "b"] "x" = ["a"; "bx"] *)
  let concat_to_last_element l x =
    let n = List.length l in
    Core.List.take l (n - 1) @ [ Core.List.last_exn l ^ x ]
  in
  (* merge_by_last_element ["a"; "b"] ["x", "y"] = ["a"; "bx"; "y"] *)
  let merge_by_last_element l1 l2 = concat_to_last_element l1 (List.hd l2) @ List.tl l2 in
  let expand acc = function
    | None s -> concat_to_last_element acc s
    | Regular w ->
      let var_name =
        let processed_var_name = process_word map w in
        if List.length processed_var_name <> 1
        then
          (* That means that we're got a situation like
             ```
             a = b c
             foo = $($(a))
             ```
             This is not legal, however, real make doesn't do anything
             to address this issue, it just ignores it. So do we
           *)
          ""
        else List.hd processed_var_name
      in
      (match VarMap.find_opt var_name map with
       | None -> acc
       | Some var_data ->
         (match var_data with
          | Expanded data -> merge_by_last_element acc data
          | Raw data ->
            let data = List.concat_map (process_word map) data in
            merge_by_last_element acc data))
    | Asterisk ->
      (match recipe_context with
       | None -> acc
       | Some context -> concat_to_last_element acc context.stem)
    | At ->
      (match recipe_context with
       | None -> acc
       | Some context -> concat_to_last_element acc context.current_target)
    | Lesser ->
      (match recipe_context with
       | None -> acc
       | Some context -> concat_to_last_element acc context.first_prerequisite)
    | Pattern ->
      (match prerequisite_context with
       | None -> concat_to_last_element acc "%"
       | Some stem -> concat_to_last_element acc stem)
  in
  List.fold_left expand [ "" ] word
;;

(* words list to string list *)
let expand_words_list map = List.concat_map (process_word map)

let string_of_recipe context map x =
  process_word ~recipe_context:(Some context) map x |> String.concat " "
;;

(* Get all filenames that matches the pattern "prefix%suffix" (like "/path/to/smth/%.c"),
 where % -- nonzero number on any symbols. Also returns so-called "stem" -- what was actually
 have been substituted in place of % *)
let get_filenames_with_stem prefix suffix =
  (* Avaliable since OCaml 4.13.0, but Graphlib requires 4.11.2 *)
  let starts_with p str =
    let len = String.length p in
    String.length str >= len && String.sub str 0 len = p
  in
  (* Avaliable since OCaml 4.13.0, but Graphlib requires 4.11.2 *)
  let ends_with e s =
    let el = String.length e in
    let sl = String.length s in
    sl >= el && String.sub s (sl - el) el = e
  in
  let dir_contents =
    let rec loop result = function
      | f :: fs when Sys.is_directory f ->
        Sys.readdir f
        |> Array.to_list
        |> List.map (Filename.concat f)
        |> List.append fs
        |> loop result
      | f :: fs -> loop (f :: result) fs
      | [] -> result
    in
    let files = loop [] [ "." ] in
    List.map (fun s -> String.sub s 2 (String.length s - 2)) files
  in
  let check_file file =
    if starts_with prefix file && ends_with suffix file
    then (
      let stem =
        String.sub
          file
          (String.length prefix)
          (String.length file - String.length prefix - String.length suffix)
      in
      Some (file, stem))
    else None
  in
  List.filter_map check_file dir_contents
;;

let process_pattern_rule map prefix suffix prerequisites recipes =
  let filenames_with_stem = get_filenames_with_stem prefix suffix in
  let filename_with_stem_to_rule (target, stem) =
    let targets = [ target ] in
    let prerequisites =
      List.concat_map (process_word ~prerequisite_context:(Some stem) map) prerequisites
    in
    let context =
      { current_target = target
      ; first_prerequisite = (if prerequisites = [] then "" else List.hd prerequisites)
      ; stem
      }
    in
    { targets; prerequisites; recipes = recipes, context }
  in
  List.map filename_with_stem_to_rule filenames_with_stem
;;

(* Check whether the first string in targets contain '%'*)
let check_pattern_presence targets = String.contains (targets |> List.hd) '%'

(** Turns rule into a list of processed rules (this could happen when target
    contains pattern).
    Expands variables in targets and prerequisites, leaving recipes intact *)
let process_rule map ({ targets; prerequisites; recipes } : rule) =
  let targets = [ fst targets ] @ snd targets in
  let targets = expand_words_list map targets in
  match String.split_on_char '%' (List.hd targets) with
  | prefix :: suffix :: _ ->
    (match targets with
     | _ :: _ :: _ ->
       print_endline
         "Currently, pattern targets with multiple targets are not supported. The first \
          target in the rule will be used as a pattern.\n\
          Other targets in that rule will be dropped."
     | _ -> ());
    process_pattern_rule map prefix suffix prerequisites recipes
  | _ ->
    let prerequisites = expand_words_list map prerequisites in
    let context =
      { current_target = List.hd targets
      ; first_prerequisite = (if prerequisites = [] then "" else List.hd prerequisites)
      ; stem = ""
      }
    in
    [ { targets; prerequisites; recipes = recipes, context } ]
;;

let process_var map var =
  let name_as_string name =
    let processed_var_name = process_word map name in
    if List.length processed_var_name <> 1
    then
      (* That means that we're got a situation like
         ```
         a = b c
         $(a) = foo
         ```
         This is not legal, however, real make doesn't do anything
         to address this issue, it just ignores it. So do we.

         Note that this is slightly different comparing to the
         same check in a "process_word" function.
       *)
      Option.None
    else if List.hd processed_var_name = ""
    then
      (* That means that we're got a situation like
         ```
         $() = foo
         ```
         Same thing, just ignore it
       *)
      Option.None
    else Some (List.hd processed_var_name)
  in
  match var with
  | Recursive (name, value) ->
    (match name_as_string name with
     | None -> map
     | Some name -> VarMap.add name (Raw value) map)
  | Simply (name, value) ->
    (match name_as_string name with
     | None -> map
     | Some name -> VarMap.add name (Expanded (expand_words_list map value)) map)
  | Conditional (name, value) ->
    (match name_as_string name with
     | None -> map
     | Some name ->
       VarMap.update
         name
         (function
          | None -> Some (Raw value)
          | Some old_value -> Some old_value)
         map)
;;

(** Traverse list of exprs, extracting rules, builing mapping for variables
    and performing substitution on targets and dependencies.
    Keeps rules order.
    Returns list of rules and mapping for variables
 *)
let processed_rules_and_vars_of_exprs exprs =
  let process_expr (rules, map) = function
    | Rule rule ->
      let result = process_rule map rule in
      rules @ result, map
    | Var var -> rules, process_var map var
  in
  List.fold_left process_expr ([], VarMap.empty) exprs
;;

(* Return true if a target is present somewhere in targets in one rule *)
let rule_has_target target rule = List.mem target rule.targets

(* Return true if a target is present somewhere in targets in list of rules *)
let rules_has_target target rules =
  Option.is_some (List.find_opt (rule_has_target target) rules)
;;

(** Return recipes for specified target among all rules.
    If two or more rules are found with the same target in the target list,
    and with different recipes, the newer one will be used
 *)
let recipes_of_target target rules =
  let rules_with_target = List.filter (rule_has_target target) rules in
  let recipes_list = List.map (fun rule -> rule.recipes) rules_with_target in
  let rec traverse_recipes = function
    | [] -> [], default_recipe_context
    | [ (x, c) ] -> x, c
    | (_, _) :: tl ->
      Printf.eprintf
        "Makefile: warning: overriding recipe for target '%s'\n\
        \ Makefile: warning: ignoring old recipe for target '%s'\n"
        target
        target;
      traverse_recipes tl
  in
  traverse_recipes recipes_list
;;

(** Return dependencies for specified target among all rules. *)
let dependencies_of_target target rules =
  let rules_with_target = List.filter (rule_has_target target) rules in
  let dependencies_list = List.map (fun rule -> rule.prerequisites) rules_with_target in
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
    ; recipes : recipe list * recipe_context
         [@compare.ignore] [@hash.ignore] [@sexp.ignore]
    }
  [@@deriving compare, hash, sexp]

  include Core.Comparable.S with type t := t
  include Core.Hashable.S with type t := t
end = struct
  module T = struct
    type t =
      { target : string
      ; recipes : recipe list * recipe_context
           [@compare.ignore] [@hash.ignore] [@sexp.ignore]
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

(* Map to get corresponding vertex in graph by name (string),
  cause Graphlib do not provide such interface. *)
type vermap = G.Node.t VertexMap.t

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
  let map = VertexMap.empty in
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

let recompile (node : G.Node.t) varmap is_default_goal =
  let recipes = node.node.recipes in
  let target = node.node.target in
  let execute_recipes =
    let rec traverse_recipes = function
      | [], _ -> ()
      | recipe :: tl, context ->
        let cmd =
          match recipe with
          | Echo x ->
            let x = string_of_recipe context varmap x in
            print_endline x;
            x
          | Silent x -> string_of_recipe context varmap x
        in
        let rc = Sys.command cmd in
        if rc <> 0 then raise (Recipe (target, rc)) else traverse_recipes (tl, context)
    in
    traverse_recipes recipes
  in
  match recipes with
  | [], _ -> if is_default_goal then raise (NothingToBeDone target)
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
let try_execute_recipes
  (node : G.Node.t)
  graph
  rules
  varmap
  nodes_to_recompile
  is_default_goal
  =
  let target = node.node.target in
  if G.Node.Set.mem nodes_to_recompile node
  then (
    recompile node varmap is_default_goal;
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
    recompile node varmap is_default_goal;
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
let build_target target map graph rules varmap =
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
            varmap
            state.nodes_to_recompile
            (target = node.node.target)
      })
;;

(* Usage: *)
(* let file = open_out_bin "mygraph.dot" in *)
(* Dot.output_graph file graph; *)
module Dot = Graph.Graphviz.Dot (struct
  module G = Graphlib.To_ocamlgraph (G)
  include G

  let edge_attributes _ = []
  let default_edge_attributes _ = []
  let get_subgraph _ = Option.None

  let vertex_attributes (_ : (Node.t, unit) labeled) =
    (* [ `Shape `Box; (if v.node_label then `Color 16711680 else `Color 0) ] *)
    [ `Shape `Box ]
  ;;

  let vertex_name (v : (Node.t, unit) labeled) = Printf.sprintf "%S" v.node.target
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

let interpret ast targets =
  let exprs = ast |> exprs_of_ast in
  let rules, varmap = processed_rules_and_vars_of_exprs exprs in
  let graph, vertex_map = graph_of_rules rules in
  let graph, vertex_map = add_default_goals targets vertex_map rules graph in
  let rec traverse_targets = function
    | [] -> Ok ""
    | target :: tl ->
      (try
         let _ = build_target target vertex_map graph rules varmap in
         traverse_targets tl
       with
       | Recipe (target, rc) ->
         Error (Printf.sprintf "make: *** [Makefile: '%s'] Error %d\n" target rc)
       | NoRule target ->
         Error (Printf.sprintf "make: *** No rule to make target '%s'. Stop.\n" target)
       | UpToDate target -> Ok (Printf.sprintf "make: '%s' is up to date.\n" target)
       | NothingToBeDone target ->
         Ok (Printf.sprintf "make: Nothing to be done for '%s'.\n" target))
  in
  match targets with
  (* When no targets are specified, choose first one *)
  | [] -> traverse_targets [ (List.hd rules).targets |> List.hd ]
  | _ -> traverse_targets targets
;;
