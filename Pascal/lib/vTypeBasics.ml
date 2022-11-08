(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

let get_type_val = function
  | VBool _ -> VTBool
  | VInt _ -> VTInt
  | VFloat _ -> VTFloat
  | VChar _ -> VTChar
  | VString s -> VTString (String.length s)
  | VRecord w -> VTDRecord (KeyMap.map (fun (t, _) -> t) w)
  | VFunction (_, t, p, _, _) -> VTFunction (p, t)
  | VArray (v, s, t, _) -> VTArray (v, s, t)
  | VVoid -> VTVoid
;;

let%test "VBool type" = get_type_val (VBool true) == VTBool

let rec compare_types : vtype -> vtype -> bool =
 fun t1 t2 ->
  match t1, t2 with
  | VTString _, VTString _ -> true
  | VTDRecord w1, VTDRecord w2 -> KeyMap.equal compare_types w1 w2
  | VTFunction (p1, t1), VTFunction (p2, t2) ->
    compare_types t1 t2
    && List.fold_left2
         (fun b p1 p2 ->
           b
           &&
           match p1, p2 with
           | FPFree (_, t1), FPFree (_, t2)
           | FPConst (_, t1), FPConst (_, t2)
           | FPOut (_, t1), FPOut (_, t2) -> compare_types t1 t2
           | _ -> false)
         true
         p1
         p2
  | VTArray (s1, i1, t1), VTArray (s2, i2, t2) ->
    s1 == s2 && i1 == i2 && compare_types t1 t2
  | t1, t2 -> t1 == t2
;;

let%test "simple type cmp" = compare_types VTBool VTBool
