(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open KeyMap
open Exceptions
open Eval
open VTypeBasics
open ImArray

let rec load_variables : define list -> world =
 fun def ->
  let var v = VVariable v in
  let const v = VConst v in
  let tp = VType in
  let def_to_world w =
    let eval_expr = eval_expr_const w in
    let rec load_type = function
      | VTString e ->
        VTDString
          (match eval_expr e with
          | VInt i when i > 0 -> i
          | _ -> raise (PascalInterp TypeError))
      | VTNDString -> VTDString 255
      | VTDString i -> if i > 0 then VTDString i else raise (PascalInterp TypeError)
      | VTRecord l ->
        let list_to_map =
          let add_to_map w (n, t) =
            match w with
            | w when not (KeyMap.mem n w) -> KeyMap.add n t w
            | _ -> raise (PascalInterp (DupVarName n))
          in
          List.fold_left add_to_map KeyMap.empty
        in
        load_type (VTDRecord (list_to_map l))
      | VTDRecord w -> VTDRecord (KeyMap.map load_type w)
      | VTFunction (p, t) -> VTFunction (load_type_fun_param p, load_type t)
      | VTArray (e1, e2, t) ->
        let v1 = eval_expr e1 in
        let v2 = eval_expr e2 in
        let size = iter_arr v1 v2 + 1 in
        load_type (VTDArray (v1, size, t))
      | VTDArray (v, s, t) ->
        if s > 0
        then (
          match get_type_val v with
          | VTInt | VTChar | VTBool -> VTDArray (v, s, load_type t)
          | _ -> raise (PascalInterp TypeError))
        else raise (PascalInterp TypeError)
      | VTCustom n ->
        (match KeyMap.find_opt n w with
        | Some (t, VType) -> t
        | Some _ -> raise (PascalInterp (NotAType n))
        | _ -> raise (PascalInterp (TypeNotFound n)))
      | VTCollable n -> raise (PascalInterp (InvalidCall n))
      | (VTBool | VTInt | VTFloat | VTChar | VTVoid) as t -> t
    and load_type_fun_param pl =
      List.map
        (function
          | FPFree (n, t) -> FPFree (n, load_type t)
          | FPOut (n, t) -> FPOut (n, load_type t)
          | FPConst (n, t) -> FPConst (n, load_type t))
        pl
    in
    let rec construct = function
      | VTBool -> VBool false
      | VTInt -> VInt 0
      | VTFloat -> VFloat 0.
      | VTChar -> VChar (Char.chr 0)
      | VTDString _ -> VString ""
      | VTDRecord w -> VRecord (KeyMap.map (fun t -> t, VVariable (construct t)) w)
      | VTFunction _ -> VVoid
      | VTDArray (v, s, t) -> VArray (v, s, t, ImArray.make s (construct t))
      | _ -> VVoid
    in
    function
    | DType (n, t) ->
      let t = load_type t in
      n, (t, tp)
    | DNDVariable (n, t) ->
      let t = load_type t in
      n, (t, var (construct t))
    | DVariable (n, t, e) ->
      let t = load_type t in
      n, (t, var (eval_expr e))
    | DDVariable (n, t, v) ->
      let t = load_type t in
      n, (t, var v)
    | DConst (n, e) ->
      let v = eval_expr e in
      n, (get_type_val v, var v)
    | DDConst (n, v) -> n, (get_type_val v, const v)
    | DFunction (n, t, p, (d, c)) ->
      let t = load_type t in
      let p = load_type_fun_param p in
      let fun_param_def =
        List.map
          (function
            | FPFree (n, t) | FPOut (n, t) -> DNDVariable (n, t)
            | FPConst (n, t) -> DDConst (n, construct t))
          p
      in
      let fdef = (DNDVariable (n, t) :: fun_param_def) @ d in
      n, (VTFunction (p, t), const (VFunction (n, t, p, load_variables fdef, c)))
  in
  let add_to_world w d =
    let name, value = def_to_world w d in
    if KeyMap.mem name w
    then raise (PascalInterp (DupVarName name))
    else KeyMap.add name value w
  in
  List.fold_left (fun w d -> add_to_world w d) KeyMap.empty def
;;
