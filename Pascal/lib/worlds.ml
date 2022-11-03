(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Exceptions

type t = world list

let load_all_opt n =
  let rec helper acc = function
    | [] -> None
    | h :: tl as w ->
      (match KeyMap.find_opt n h with
       | Some v -> Some (acc, (n, v), w)
       | None -> helper (h :: acc) tl)
  in
  helper []
;;

let load_all n w =
  match load_all_opt n w with
  | Some v -> v
  | _ -> raise (PascalInterp (VariableNotFound n))
;;

let load_opt n = List.find_map (KeyMap.find_opt n)

let load n worlds =
  match load_opt n worlds with
  | Some v -> v
  | _ -> raise (PascalInterp (VariableNotFound n))
;;

let replace n v w =
  let rec helper acc = function
    | h :: tl when KeyMap.mem n h -> acc, KeyMap.add n v h :: tl
    | h :: tl -> helper (h :: acc) tl
    | [] -> raise (PascalInterp (VariableNotFound n))
  in
  let h, tl = helper [] w in
  List.rev_append h tl
;;

let mem n = List.exists (KeyMap.mem n)

let root =
  let rec helper acc = function
    | [] -> [], []
    | r :: [] -> acc, [ r ]
    | h :: tl -> helper (h :: acc) tl
  in
  helper []
;;
