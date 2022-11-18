(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Exceptions
open VTypeBasics

type t = world list

let load_opt n = List.find_map (KeyMap.find_opt n)

let load n worlds =
  match load_opt n worlds with
  | Some v -> v
  | _ -> raise (PascalInterp (VariableNotFound n))
;;

let replace : name -> vtype * value -> t -> t =
 fun n (t, v) w ->
  let rec helper acc = function
    | h :: tl ->
      (match KeyMap.find_opt n h with
       | Some (ht, VVariable _) when cast_type ht t ->
         acc, KeyMap.add n (ht, VVariable (cast ht v)) h :: tl
       | Some (ht, VFunctionResult _) when cast_type ht t ->
         acc, KeyMap.add n (ht, VFunctionResult (cast ht v)) h :: tl
       | Some _ -> raise (PascalInterp (NotAVariable n))
       | None -> helper (h :: acc) tl)
    | [] -> raise (PascalInterp (VariableNotFound n))
  in
  let h, tl = helper [] w in
  List.rev_append h tl
;;

let mem n = List.exists (KeyMap.mem n)

let root w =
  let rec helper acc = function
    | [] -> [], []
    | r :: [] -> acc, [ r ]
    | h :: tl -> helper (h :: acc) tl
  in
  helper [] w
;;

let load_const_fun_opt n w =
  match root w with
  | _, h :: [] ->
    (match KeyMap.find_opt n h with
     | Some ((VTConstFunction _ as t), VConst f) -> Some (t, f)
     | _ -> None)
  | _ -> None
;;

let load_const_fun n w =
  match load_const_fun_opt n w with
  | Some f -> f
  | _ -> raise (PascalInterp (VariableNotFound n))
;;

let load_fun_all_opt n w =
  let rec helper acc = function
    | [] -> None
    | h :: tl as w ->
      (match KeyMap.find_opt n h with
       | Some v -> Some (acc, (n, v), w)
       | None -> helper (h :: acc) tl)
  in
  match helper [] w with
  | Some (wh, (_, ((VTConstFunction _ as t), VConst v)), wtl) -> Some (wh, (t, v), wtl)
  | Some (_, (_, ((VTFunction _ as t), VVariable v)), _) ->
    let wh, wtl = root w in
    Some (wh, (t, v), wtl)
  | Some (wh, (_, (_, VFunctionResult _)), h :: (hwf :: _ as wtl)) ->
    (match KeyMap.find_opt n hwf with
     | Some ((VTConstFunction _ as t), VConst v) ->
       let wh = h :: wh in
       Some (wh, (t, v), wtl)
     | _ -> None)
  | Some _ -> raise (PascalInterp (NotAFunction n))
  | _ -> None
;;

let load_fun_opt n w =
  match load_fun_all_opt n w with
  | Some (_, f, _) -> Some f
  | _ -> None
;;

let load_fun n w =
  match load_fun_opt n w with
  | Some f -> f
  | _ -> raise (PascalInterp (FunctionNotFound n))
;;
