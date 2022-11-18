(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Exceptions
open Eval
open VTypeBasics

let load_variables def =
  let rec load_variables_in def w =
    let var v = VVariable v in
    let const v = VConst v in
    let tp = VType in
    let def_to_world w =
      let eval_expr e = eval_expr_const e w in
      let rec load t =
        let rec eval = function
          | PTBool -> VTBool
          | PTInt -> VTInt
          | PTFloat -> VTFloat
          | PTChar -> VTChar
          | PTVoid -> VTVoid
          | PTDString e ->
            VTString
              (match eval_expr e with
               | VInt i when i > 0 -> i
               | _ -> raise (PascalInterp TypeError))
          | PTString -> VTString 255
          | PTRecord l ->
            let list_to_map =
              let add_to_map w (n, t) =
                match w with
                | w when not (KeyMap.mem n w) -> KeyMap.add n (load t) w
                | _ -> raise (PascalInterp (DupVarName n))
              in
              List.fold_left add_to_map KeyMap.empty
            in
            VTRecord (list_to_map l)
          | PTFunction (p, t) -> VTFunction (load_fun_param p, load t)
          | PTArray (e1, e2, t) ->
            let v1 = eval_expr e1 in
            let v2 = eval_expr e2 in
            let size = iter_arr v1 v2 + 1 in
            VTArray (v1, size, eval t)
          | PTCustom n ->
            (match Worlds.load n w with
             | t, VType -> t
             | _ -> raise (PascalInterp (NotAType n)))
        in
        match eval t with
        | VTString i when i <= 0 -> raise (PascalInterp TypeError)
        | VTArray ((VChar _ | VInt _ | VBool _), s, _) when s <= 0 ->
          raise (PascalInterp TypeError)
        | ok -> ok
      and load_fun_param pl =
        List.map
          (function
           | FPFree (n, t) -> FPFree (n, load t)
           | FPOut (n, t) -> FPOut (n, load t)
           | FPConst (n, t) -> FPConst (n, load t))
          pl
      in
      let rec construct = function
        | VTBool -> VBool false
        | VTInt -> VInt 0
        | VTFloat -> VFloat 0.
        | VTChar -> VChar (Char.chr 0)
        | VTVoid -> VVoid
        | VTString i -> VString ("", i)
        | VTRecord w -> VRecord (KeyMap.map (fun t -> t, VVariable (construct t)) w)
        | VTFunction _ -> VVoid
        | VTConstFunction _ -> VVoid
        | VTArray (v, s, t) -> VArray (v, s, t, ImArray.make s (construct t))
      in
      function
      | DType (n, t) ->
        let t = load t in
        n, (t, tp)
      | DVariable (n, t) ->
        let t = load t in
        n, (t, var (construct t))
      | DDVariable (n, t, e) ->
        let t = load t in
        n, (t, var (eval_expr e))
      | DConst (n, e) ->
        let v = eval_expr e in
        n, (get_type_val v, const v)
      | DDConst (n, v) -> n, (get_type_val v, const v)
      | DFunction (n, t, p, (d, c)) ->
        let t = load t in
        let fun_param_def =
          List.map
            (function
             | FPFree (n, t) | FPOut (n, t) -> DVariable (n, t)
             | FPConst (n, t) -> DDConst (n, construct (load t)))
            p
        in
        let p = load_fun_param p in
        let fdef = fun_param_def @ d in
        let fw = load_variables_in fdef (KeyMap.empty :: w) in
        let fw =
          if KeyMap.mem n fw
          then raise (PascalInterp (DupVarName n))
          else KeyMap.add n (t, VFunctionResult (construct t)) fw
        in
        n, (VTConstFunction (p, t), const (VFunction (n, t, p, fw, c)))
    in
    let add_to_world w d =
      let name, (t, v) = def_to_world w d in
      let v =
        match v with
        | VConst (VFunction _) as v -> v
        | VVariable v -> VVariable (cast t v)
        | VConst v -> VConst (cast t v)
        | VFunctionResult v -> VConst (cast t v)
        | VType -> VType
      in
      match w with
      | [] -> [ KeyMap.add name (t, v) KeyMap.empty ]
      | h :: _ when KeyMap.mem name h -> raise (PascalInterp (DupVarName name))
      | h :: tl -> KeyMap.add name (t, v) h :: tl
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
       ; DVariable ("a", PTArray (Const (VInt 0), Variable "n", PTBool))
       ; DFunction
           ( "f"
           , PTBool
           , []
           , ( [ DVariable ("a", PTArray (Const (VInt 0), Variable "n", PTBool))
               ; DConst ("n", BinOp (Add, Const (VInt 5), Variable "n"))
               ; DFunction
                   ( "ff"
                   , PTBool
                   , []
                   , ( [ DVariable ("a", PTArray (Const (VInt 0), Variable "n", PTBool)) ]
                     , [] ) )
               ]
             , [] ) )
       ; DConst ("nn", Variable "n")
       ])
    (KeyMap.of_seq
       (List.to_seq
          [ "n", (VTInt, VConst (VInt 4))
          ; ( "a"
            , ( VTArray (VInt 0, 5, VTBool)
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
                              , ( VTArray (VInt 0, 5, VTBool)
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
                                                , ( VTArray (VInt 0, 10, VTBool)
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
