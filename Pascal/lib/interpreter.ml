(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Eval
open Exceptions
open VarLoader
open VTypeBasics
open Parser
open Monadic

let rec is_l_value e w =
  match e with
  | GetArr (e, _) | GetRec (e, _) -> is_l_value e w
  | Variable n ->
    (match Worlds.load n w with
     | _, (VVariable _ | VFunctionResult _) -> true
     | _, (VConst _ | VType) -> false)
  | _ -> false
;;

let rec eval_expr_type : expr -> Worlds.t -> vtype =
 fun e w ->
  let eval_expr e = eval_expr_type e w in
  let loader n w =
    let _, (_, (t, v)), wtl = Worlds.load_all n w in
    match t, v with
    | VTFunction _, VConst _ ->
      (match wtl with
       | _ :: [] -> t
       | _ -> raise (PascalInterp (GlobalFunctionExpected n)))
    | _, (VVariable _ | VConst _ | VFunctionResult _) -> t
    | _, VType -> raise (PascalInterp (NotAVariable n))
  in
  let eval_function f p w =
    let compare_param_list =
      List.for_all2 (fun p ep ->
        let tep = eval_expr ep in
        match p with
        | FPFree (_, tp) | FPConst (_, tp) -> compare_types tp tep
        | FPOut (_, tp) -> is_l_value ep w && compare_types tp tep)
    in
    match f with
    | Variable fn ->
      (match Worlds.load_all_opt fn w with
       | Some (_, (_, (_, VFunctionResult _)), _ :: h :: _) ->
         (match KeyMap.find_opt fn h with
          | Some (VTFunction (fpl, t), _) when compare_param_list fpl p -> return t w
          | _ -> raise (PascalInterp (InvalidCall fn)))
       | Some (_, (_, (_, VFunctionResult _)), _) -> raise (PascalInterp (InvalidCall fn))
       | Some (_, (_, (VTFunction (fpl, t), _)), _) when compare_param_list fpl p ->
         return t w
       | None ->
         let p = List.map (fun e -> eval_expr e) p in
         return (eval_std_function_type fn p) w
       | _ -> raise (PascalInterp (InvalidCall fn)))
    | _ -> raise (PascalInterp TypeError)
  in
  use (eval_expr_base_type loader eval_function e) w
;;

let ( >> ) _ b = b

let rec eval_stmt_type ?(func = false) ?(loop = false) s w =
  let eval_expr e = eval_expr_type e w in
  let eval_stmt_list sl = eval_stmt_list_type ~func ~loop sl w in
  let eval_stmt_list_loop sl = eval_stmt_list_type ~func ~loop:true sl w in
  match s with
  | Assign (l, r) when is_l_value l w -> compare_types (eval_expr l) (eval_expr r)
  | Assign _ -> raise (PascalInterp LeftValError)
  | ProcCall (Call _ as e) -> eval_expr e >> true
  | ProcCall _ -> false
  | If (b, t, e) ->
    compare_types (eval_expr b) VTBool && eval_stmt_list t && eval_stmt_list e
  | While (b, t) | Repeat (b, t) ->
    compare_types (eval_expr b) VTBool && eval_stmt_list_loop t
  | For (n, s, f, t) ->
    let nt, _ = Worlds.load n w in
    compare_types nt (eval_expr s)
    && compare_types nt (eval_expr f)
    && eval_stmt_list_loop t
  | Break | Continue -> loop
  | Exit -> func

and eval_stmt_list_type ?(func = false) ?(loop = false) sl w =
  List.for_all (fun s -> eval_stmt_type ~func ~loop s w) sl
;;

let semantic_test =
  let rec semantic_test_in func c = function
    | h :: _ as w ->
      KeyMap.for_all
        (fun _ -> function
          | VTFunction _, VConst (VFunction (_, _, _, fw, fc)) ->
            semantic_test_in true fc (fw :: w)
          | _ -> true)
        h
      && eval_stmt_list_type ~func c w
    | _ -> false
  in
  semantic_test_in false
;;

exception BreakEx of Worlds.t
exception ContinueEx of Worlds.t
exception ExitEx of Worlds.t

let rec eval_expr e w =
  let loader n w =
    let _, v = Worlds.load n w in
    match v with
    | VVariable v | VConst v | VFunctionResult v -> v
    | VType -> raise (PascalInterp RunTimeError)
  in
  let eval_function n fp pexp p fw st w =
    let fw =
      List.fold_left2
        (fun w fp p ->
          let n =
            match fp with
            | FPFree (n, _) | FPOut (n, _) | FPConst (n, _) -> n
          in
          match Worlds.load n [ w ] with
          | t, VConst _ -> KeyMap.add n (t, VConst p) w
          | t, VVariable _ -> KeyMap.add n (t, VVariable p) w
          | _, VFunctionResult _ -> raise (PascalInterp RunTimeError)
          | _, VType -> raise (PascalInterp (NotAVariable n)))
        fw
        fp
        p
    in
    let fw, w =
      match
        try eval_stmt_list st (fw :: w) with
        | ExitEx w -> w
      with
      | h :: tl -> h, tl
      | [] -> raise (PascalInterp RunTimeError)
    in
    let load n w =
      match Worlds.load n [ w ] with
      | _, (VVariable v | VConst v | VFunctionResult v) -> v
      | _, VType -> raise (PascalInterp RunTimeError)
    in
    let w =
      List.fold_left2
        (fun w fp p ->
          match fp with
          | FPOut (n, _) -> eval_stmt (Assign (p, Const (load n fw))) w
          | _ -> w)
        w
        fp
        pexp
    in
    load n fw, w
  in
  let eval_function f pexp w =
    let w, p = List.fold_left_map (swap eval_expr) w pexp in
    match f with
    | Variable f ->
      (match Worlds.load_all_opt f w with
       | Some (wh, (_, (_, VConst (VFunction (n, _, fp, fw, st)))), wtl) ->
         let r, wtl = eval_function n fp pexp p fw st wtl in
         r, List.rev_append wh wtl
       | Some (_, (_, (_, VVariable (VFunction (n, _, fp, fw, st)))), _) ->
         let wh, wtl = Worlds.root w in
         let r, wtl = eval_function n fp pexp p fw st wtl in
         r, List.rev_append wh wtl
       | Some (wh, (_, (_, VFunctionResult _)), h :: (hwf :: _ as wtl)) ->
         (match KeyMap.find_opt f hwf with
          | Some (_, VConst (VFunction (n, _, fp, fw, st))) ->
            let wh = h :: wh in
            let r, wtl = eval_function n fp pexp p fw st wtl in
            r, List.rev_append wh wtl
          | _ -> raise (PascalInterp RunTimeError))
       | Some _ -> raise (PascalInterp (InvalidCall f))
       | None -> eval_std_function f p, w)
    | e ->
      let f, w = eval_expr e w in
      (match f with
       | VFunction (n, _, fp, fw, st) ->
         let wh, wtl = Worlds.root w in
         let r, wtl = eval_function n fp pexp p fw st wtl in
         r, List.rev_append wh wtl
       | _ -> raise (PascalInterp (CantCall f)))
  in
  eval_expr_base loader eval_function e w

and eval_stmt s w =
  let rec loop e f sl w =
    let b, w = eval_expr e w in
    match b with
    | VBool b when f b ->
      (match
         try Ok (eval_stmt_list sl w) with
         | BreakEx w -> Error w
         | ContinueEx w -> Ok w
       with
       | Error w -> w
       | Ok w -> loop e f sl w)
    | VBool _ -> w
    | _ -> raise (PascalInterp RunTimeError)
  in
  let iter v step =
    match v with
    | VInt i -> VInt (i + step)
    | VChar c -> VChar (Char.chr (Char.code c + step))
    | VBool b ->
      (match Bool.to_int b + step with
       | 0 -> VBool false
       | 1 -> VBool true
       | _ -> raise (PascalInterp RunTimeError))
    | _ -> raise (PascalInterp RunTimeError)
  in
  match s with
  | Assign (Variable n, e) ->
    (match Worlds.load n w with
     | t, VVariable _ ->
       let v, w = eval_expr e w in
       Worlds.replace n (t, VVariable v) w
     | t, VFunctionResult _ ->
       let v, w = eval_expr e w in
       Worlds.replace n (t, VFunctionResult v) w
     | _ -> raise (PascalInterp (NotAVariable n)))
  | Assign (l, r) ->
    let rec helper w f =
      let arr_add a i v =
        match a with
        | VArray (s, l, t, a) -> VArray (s, l, t, ImArray.set a (iter_arr s i) v)
        | _ -> raise (PascalInterp RunTimeError)
      in
      let rec_add r n v =
        match r with
        | VRecord w ->
          (match Worlds.load n [ w ] with
           | t, VVariable _ -> VRecord (KeyMap.add n (t, VVariable v) w)
           | _ -> raise (PascalInterp RunTimeError))
        | _ -> raise (PascalInterp RunTimeError)
      in
      function
      | GetArr (e, i) ->
        let i, w = eval_expr i w in
        helper w (fun a v -> arr_add a i (f (get_arr i a) v)) e
      | GetRec (e, n) -> helper w (fun a v -> rec_add a n (f (get_rec n a) v)) e
      | Variable n ->
        (match Worlds.load n w with
         | t, VVariable v ->
           let e, w = eval_expr r w in
           Worlds.replace n (t, VVariable (f v e)) w
         | _ -> raise (PascalInterp RunTimeError))
      | _ -> raise (PascalInterp RunTimeError)
    in
    helper w (fun _ v -> v) l
  | ProcCall e ->
    let _, w = eval_expr e w in
    w
  | If (e, ts, es) ->
    let b, w = eval_expr e w in
    (match b with
     | VBool b when b -> eval_stmt_list ts w
     | VBool _ -> eval_stmt_list es w
     | _ -> raise (PascalInterp (InvalidType (VTBool, get_type_val b))))
  | While (e, st) -> loop e (fun b -> b) st w
  | Repeat (e, st) -> loop e not st w
  | For (n, s, f, st) ->
    let s, w = eval_expr s w in
    let f, w = eval_expr f w in
    let dir =
      match eval_binop Less s f with
      | VBool b -> if b then 1 else -1
      | _ -> raise (PascalInterp RunTimeError)
    in
    let rec helper i w =
      let update_n w =
        match Worlds.load n w with
        | t, VVariable _ -> Worlds.replace n (t, VVariable i) w
        | t, VFunctionResult _ -> Worlds.replace n (t, VFunctionResult i) w
        | _ -> raise (PascalInterp RunTimeError)
      in
      let w = update_n w in
      match
        try
          let w = eval_stmt_list st w in
          let w = update_n w in
          if f = i then Error w else Ok w
        with
        | BreakEx w -> Error w
        | ContinueEx w -> Ok w
      with
      | Error w -> w
      | Ok w -> helper (iter i dir) w
    in
    helper s w
  | Break -> raise (BreakEx w)
  | Continue -> raise (ContinueEx w)
  | Exit -> raise (ExitEx w)

and eval_stmt_list sl w = List.fold_left (fun w st -> eval_stmt st w) w sl

let interpret_no_catch s =
  match parse s with
  | None -> raise (PascalInterp ParserError)
  | Some (w, st) ->
    let w = [ load_variables w ] in
    if semantic_test st w
    then (
      match eval_stmt_list st w with
      | w :: [] -> w
      | _ -> raise (PascalInterp RunTimeError))
    else raise (PascalInterp SemanticError)
;;

let interpret s =
  try Ok (interpret_no_catch s) with
  | err -> Error err
;;

exception ExpFnd of name * variable * variable

let check_interp s l =
  let w = interpret_no_catch s in
  List.map
    (fun (n, v) ->
      Worlds.load n [ w ]
      |> function
      | _, wv when not (v = wv) -> raise (ExpFnd (n, v, wv))
      | _ -> true)
    l
  |> fun _ -> true
;;

let%test "2 + 2 * 2" =
  check_interp
    {|
      var
        x : integer;
      begin
        x := 2 + 2 * 2;
      end.
    |}
    [ "x", VVariable (VInt 6) ]
;;

let%test "(2 + 2) * 2" =
  check_interp
    {|
      var
        x : integer;
      begin
        x := (2 + 2) * 2;
      end.
    |}
    [ "x", VVariable (VInt 8) ]
;;

let%test "simple prog" =
  check_interp
    {|
      var
        x, y : integer;
        function add (x, y : integer) : integer;
        begin
          add := x + y;
        end;
      begin
        x := 32;
        y := 10;
        x := x + y;
      end.
    |}
    [ "x", VVariable (VInt 42); "y", VVariable (VInt 10) ]
;;

let%test "simple prog with call" =
  check_interp
    {|
      var
        x, y : integer;
        function add (x, y : integer) : integer;
        begin
          add := x + y;
        end;
      begin
        x := 32;
        y := 10;
        x := add(x, y);
      end.
    |}
    [ "x", VVariable (VInt 42); "y", VVariable (VInt 10) ]
;;

let%test "simple prog with call out" =
  check_interp
    {|
      var
        x, y : integer;
        function add (out x : integer; y : integer) : integer;
        begin
          x := x + y;
          add := x;
        end;
      begin
        x := 32;
        y := 10;
        add(x, y);
      end.
    |}
    [ "x", VVariable (VInt 42); "y", VVariable (VInt 10) ]
;;

let%test "array rec" =
  check_interp
    {|
      var
        x, y, z : integer;
        a : array [0..1, 'a'..'b'] of record
          f : integer;
        end;
      begin
        a[0, 'a'].f := 42;
        a[1]['b'].f := 41;
        a[1]['a'].f := 1;
        x := a[0]['a'].f;
        y := a[1, 'b'].f;
        z := a[1]['a'].f;
      end.
    |}
    [ "x", VVariable (VInt 42); "y", VVariable (VInt 41); "z", VVariable (VInt 1) ]
;;

let%test "func" =
  check_interp
    {|
      var
        x : integer;
        function f : integer;
        begin
          f := 42;
        end;
      begin
        x := f();
      end.
    |}
    [ "x", VVariable (VInt 42) ]
;;

let%test "func as arg" =
  check_interp
    {|
      type
        int_f = function : integer;
      var
        x : integer;
        f : int_f;
        function some_f : integer;
        begin
          some_f := 42;
        end;
      begin
        f := some_f;
        x := f();
      end.
    |}
    [ "x", VVariable (VInt 42) ]
;;

let%test "for loop" =
  check_interp
    {|
      var
        i : integer;
        x : integer = 0;
      begin
        for i := 0 to 10 do x := x + 1;
      end.
    |}
    [ "x", VVariable (VInt 11); "i", VVariable (VInt 10) ]
;;

let%test "for loop" =
  check_interp
    {|
      var
        i : integer;
        x : integer = 0;
      begin
        for i := 0 to 10 do x := i;
      end.
    |}
    [ "x", VVariable (VInt 10); "i", VVariable (VInt 10) ]
;;

let%test "for loop if _ then break" =
  check_interp
    {|
      var
        i : integer;
        x : integer = 0;
      begin
        for i := 0 to 10 do
          if x = 5
          then break
          else x := i;
        end.
    |}
    [ "x", VVariable (VInt 5); "i", VVariable (VInt 6) ]
;;

let%test "fibonacci" =
  check_interp
    {|
      var
        a, b, c, i, n, x : integer;
      begin
        a := 0;
        b := 1;
        n := 10;
        for i := 3 to n do begin
          x := a + b;
          c := b;
          b := a + b;
          a := c;
          i := 42;
        end;
      end.
    |}
    [ "x", VVariable (VInt 34); "i", VVariable (VInt 10) ]
;;

let%test "fibonacci rec" =
  check_interp
    {|
      const n = 10;
      var
        i : integer;
        function f (n : integer) : integer;
        begin
          if n = 0 then
          begin
            f := 0;
            exit;
          end else if n = 1 then
          begin
            f := 1;
            exit;
          end else
            f := f(n - 1) + f(n - 2);
        end;
      begin
        i := f(n);
      end.
    |}
    [ "i", VVariable (VInt 55) ]
;;
