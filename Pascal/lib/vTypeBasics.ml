(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Exceptions

let get_type_val = function
  | VBool _ -> VTBool
  | VInt _ -> VTInt
  | VFloat _ -> VTFloat
  | VChar _ -> VTChar
  | VString (_, i) -> VTString i
  | VRecord w -> VTRecord (KeyMap.map (fun (t, _) -> t) w)
  | VFunction (_, t, p, _, _) -> VTFunction (p, t)
  | VArray (v, s, t, _) -> VTArray (v, s, t)
  | VVoid -> VTVoid
;;

let%test "VBool type" = get_type_val (VBool true) == VTBool

let rec compare_types t1 t2 =
  let eval = function
    | VTConstFunction (t, p) -> VTFunction (t, p)
    | ok -> ok
  in
  match eval t1, eval t2 with
  | VTString _, VTString _ -> true
  | VTRecord w1, VTRecord w2 -> KeyMap.equal compare_types w1 w2
  | VTFunction (p1, t1), VTFunction (p2, t2) ->
    compare_types t1 t2
    && List.for_all2
         (fun p1 p2 ->
           match p1, p2 with
           | FPFree (_, t1), FPFree (_, t2)
           | FPConst (_, t1), FPConst (_, t2)
           | FPOut (_, t1), FPOut (_, t2) -> compare_types t1 t2
           | _ -> false)
         p1
         p2
  | VTArray (s1, i1, t1), VTArray (s2, i2, t2) ->
    s1 = s2 && i1 = i2 && compare_types t1 t2
  | t1, t2 -> t1 = t2
;;

let%test "simple type cmp" = compare_types VTBool VTBool

let cast t v =
  match t, v with
  | VTFunction _, VVoid -> VVoid
  | VTFloat, VInt v -> VFloat (Int.to_float v)
  | VTString i, VString (s, _) when String.length s > i -> VString (String.sub s 0 i, i)
  | VTString i, VString (s, _) -> VString (s, i)
  | VTString i, VChar c when i > 0 -> VString (String.make 1 c, i)
  | VTString i, VArray (_, vsz, VTChar, v) ->
    let s =
      List.fold_left
        (fun (i, s) -> function
          | VChar c when i > 0 -> i - 1, s ^ String.make 1 c
          | _ -> i, s)
        (min i vsz, "")
        (ImArray.to_list v)
      |> fun (_, s) -> s
    in
    VString (s, i)
  | VTArray (ts, tsz, VTChar), VString (s, _) ->
    let _, content_list =
      Seq.fold_left
        (fun (i, l) -> function
          | c when i > 0 -> i - 1, VChar c :: l
          | _ -> i, l)
        (min (String.length s) tsz, [])
        (String.to_seq s)
    in
    let arr =
      ImArray.of_list
        (List.rev
           (List.rev_append
              (List.init (max (tsz - String.length s) 0) (fun _ -> VChar (Char.chr 0)))
              content_list))
    in
    VArray (ts, tsz, VTChar, arr)
  | t, v when compare_types t (get_type_val v) -> v
  | _ -> raise (PascalInterp (CantCast (v, t)))
;;

let cast_type t vt =
  match t, vt with
  | VTFloat, VTInt
  | VTString _, VTArray (_, _, VTChar)
  | VTArray (_, _, VTChar), VTString _ -> true
  | VTString i, VTChar when i > 0 -> true
  | _, VTConstFunction _ -> false
  | t, vt when compare_types t vt -> true
  | _ -> false
;;
