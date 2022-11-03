(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Exceptions
open Eval
open VTypeBasics

let load_variables : define list -> world =
 fun def ->
  let rec load_variables_in def w =
    let var v = VVariable v in
    let const v = VConst v in
    let tp = VType in
    let def_to_world w =
      let eval_expr e = eval_expr_const e w in
      let rec load_type = function
        | VTString e ->
          VTDString
            (match eval_expr e with
             | VInt i when i > 0 -> i
             | _ -> raise (PascalInterp TypeError))
        | VTNDString -> VTDString 255
        | VTDString i when i > 0 -> VTDString i
        | VTDString _ -> raise (PascalInterp TypeError)
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
        | VTDArray (((VChar _ | VInt _ | VBool _) as v), s, t) when s > 0 ->
          VTDArray (v, s, load_type t)
        | VTDArray _ -> raise (PascalInterp TypeError)
        | VTCustom n ->
          (match Worlds.load n w with
           | t, VType -> t
           | _ -> raise (PascalInterp (NotAType n)))
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
        n, (get_type_val v, const v)
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
        let fdef = fun_param_def @ d in
        let fw = load_variables_in fdef (KeyMap.empty :: w) in
        let fw =
          if KeyMap.mem n fw
          then raise (PascalInterp (DupVarName n))
          else KeyMap.add n (t, VFunctionResult (construct t)) fw
        in
        n, (VTFunction (p, t), const (VFunction (n, t, p, fw, c)))
    in
    let add_to_world w d =
      let name, value = def_to_world w d in
      match w with
      | [] -> [ KeyMap.add name value KeyMap.empty ]
      | h :: _ when KeyMap.mem name h -> raise (PascalInterp (DupVarName name))
      | h :: tl -> KeyMap.add name value h :: tl
    in
    match List.fold_left (fun w d -> add_to_world w d) w def with
    | [] -> KeyMap.empty
    | h :: _ -> h
  in
  load_variables_in def [ KeyMap.empty ]
;;

let%test "load variables test" =
  let rec world_cmp x y =
    match x, y with
    | ( (xft, VConst (VFunction (xn, xt, xpl, xw, xstmt)))
      , (yft, VConst (VFunction (yn, yt, ypl, yw, ystmt))) )
      when compare_types xft yft
           && xn = yn
           && compare_types xt yt
           && xpl = ypl
           && xstmt = ystmt -> KeyMap.equal world_cmp xw yw
    | x, y -> x = y
  in
  KeyMap.equal
    world_cmp
    (load_variables
       [ DConst ("n", BinOp (Add, Const (VInt 2), Const (VInt 2)))
       ; DNDVariable ("a", VTArray (Const (VInt 0), Variable "n", VTBool))
       ; DFunction
           ( "f"
           , VTBool
           , []
           , ( [ DNDVariable ("a", VTArray (Const (VInt 0), Variable "n", VTBool))
               ; DConst ("n", BinOp (Add, Const (VInt 5), Variable "n"))
               ; DFunction
                   ( "ff"
                   , VTBool
                   , []
                   , ( [ DNDVariable ("a", VTArray (Const (VInt 0), Variable "n", VTBool))
                       ]
                     , [] ) )
               ]
             , [] ) )
       ; DConst ("nn", Variable "n")
       ])
    (KeyMap.of_seq
       (List.to_seq
          [ "n", (VTInt, VConst (VInt 4))
          ; ( "a"
            , ( VTDArray (VInt 0, 5, VTBool)
              , VVariable (VArray (VInt 0, 5, VTBool, ImArray.make 5 (VBool false))) ) )
          ; ( "f"
            , ( VTFunction ([], VTBool)
              , VConst
                  (VFunction
                     ( "f"
                     , VTBool
                     , []
                     , KeyMap.of_seq
                         (List.to_seq
                            [ "f", (VTBool, VFunctionResult (VBool false))
                            ; ( "a"
                              , ( VTDArray (VInt 0, 5, VTBool)
                                , VVariable
                                    (VArray
                                       (VInt 0, 5, VTBool, ImArray.make 5 (VBool false)))
                                ) )
                            ; "n", (VTInt, VConst (VInt 9))
                            ; ( "ff"
                              , ( VTFunction ([], VTBool)
                                , VConst
                                    (VFunction
                                       ( "ff"
                                       , VTBool
                                       , []
                                       , KeyMap.of_seq
                                           (List.to_seq
                                              [ ( "ff"
                                                , (VTBool, VFunctionResult (VBool false))
                                                )
                                              ; ( "a"
                                                , ( VTDArray (VInt 0, 10, VTBool)
                                                  , VVariable
                                                      (VArray
                                                         ( VInt 0
                                                         , 10
                                                         , VTBool
                                                         , ImArray.make 10 (VBool false)
                                                         )) ) )
                                              ])
                                       , [] )) ) )
                            ])
                     , [] )) ) )
          ; "nn", (VTInt, VConst (VInt 4))
          ]))
;;
