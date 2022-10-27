(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open KeyMap
open Eval
open Exceptions
open VarLoader
open VTypeBasics
open ImArray
open Parser

let ( => ) arg f = f arg

let ( >> ) _ arg = arg

let load : world -> name -> vtype * variable =
 fun w n ->
  match KeyMap.find_opt n w with
  | Some d -> d
  | _ -> raise (PascalInterp (VariableNotFound n))

let rec is_l_value w = function
  | GetArr (e, _) | GetRec (e, _) -> is_l_value w e
  | Variable n -> ( match load w n with _, VVariable _ -> true | _ -> false)
  | _ -> false

let rec eval_expr_type w e =
  let loader w n =
    match KeyMap.find_opt n w with Some (t, _) -> t | None -> VTCollable n
  in
  let eval_function w t p =
    let compare_param_list =
      List.fold_left2
        (fun b p ep ->
          b
          &&
          let tep = eval_expr_type w ep in
          match p with
          | FPFree (_, tp) | FPConst (_, tp) -> compare_types tp tep
          | FPOut (_, tp) -> is_l_value w ep && compare_types tp tep)
        true
    in
    match t with
    | VTFunction (fp, t) when compare_param_list fp p -> t
    | VTCollable n -> eval_std_function_type n (List.map (eval_expr_type w) p)
    | _ -> raise (PascalInterp TypeError)
  in
  eval_expr_base_type loader eval_function w e

let rec eval_stmt_type ?(loop = false) w =
  let eval_expr = eval_expr_type w in
  let eval_stmt_list = eval_stmt_list_type ~loop w in
  let eval_stmt_list_loop = eval_stmt_list_type ~loop:true w in
  function
  | Assign (l, r) ->
      if not (is_l_value w l) then raise (PascalInterp LeftValError)
      else compare_types (eval_expr l) (eval_expr r)
  | ProcCall (Call _ as e) -> eval_expr e >> true
  | If (b, t, e) ->
      compare_types (eval_expr b) VTBool && eval_stmt_list t && eval_stmt_list e
  | While (b, t) | Repeat (b, t) ->
      compare_types (eval_expr b) VTBool && eval_stmt_list_loop t
  | For (n, s, f, t) ->
      let nt, _ = load w n in
      compare_types nt (eval_expr s)
      && compare_types nt (eval_expr f)
      && eval_stmt_list_loop t
  | Break | Continue -> loop
  | _ -> false

and eval_stmt_list_type ?(loop = false) w =
  List.fold_left (fun b st -> b && eval_stmt_type ~loop w st) true

let rec semantic_test : world -> statement list -> bool =
 fun w c ->
  KeyMap.fold
    (fun _ v b ->
      match v with
      | VTFunction _, VConst (VFunction (_, _, _, w, c)) -> semantic_test w c
      | _ -> b)
    w true
  && eval_stmt_list_type w c

exception BreakEx of world

exception ContinueEx of world

let rec eval_expr w e =
  let rec eval_expr_in w =
    let eval_expr w = eval_expr_in w in
    function
    | Const v -> (w, v)
    | Variable n -> (
        match KeyMap.find_opt n w with
        | Some (_, VConst v) | Some (_, VVariable v) -> (w, v)
        | None -> (w, VCollable n)
        | _ -> raise (PascalInterp (NotAVariable n)))
    | BinOp (op, x, y) ->
        let w, x = eval_expr w x in
        let w, y = eval_expr w y in
        (w, eval_binop op x y)
    | UnOp (op, x) ->
        let w, x = eval_expr w x in
        (w, eval_unop op x)
    | Call (e, p) -> (
        let w, f = eval_expr w e in
        let w, pv = List.fold_left_map eval_expr w p in
        match f with
        | VCollable n -> (w, eval_std_function n pv)
        | VFunction (n, _, fp, fw, st) -> (
            let rec helper fp pv w =
              match (fp, pv) with
              | ( (FPFree (n, _) | FPOut (n, _) | FPConst (n, _)) :: fptl,
                  p :: ptl ) ->
                  let w =
                    match load w n with
                    | t, VConst _ -> KeyMap.add n (t, VConst p) w
                    | t, VVariable _ -> KeyMap.add n (t, VVariable p) w
                    | _ -> raise (PascalInterp (NotAVariable n))
                  in
                  helper fptl ptl w
              | [], [] -> w
              | _ -> raise (PascalInterp RunTimeError)
            in
            let fw = helper fp pv fw in
            let fw = eval_stmt_list fw st in
            let rec helper fp p w =
              match (fp, p) with
              | FPOut (n, _) :: fptl, p :: ptl ->
                  let w =
                    match load fw n with
                    | _, (VVariable v | VConst v) ->
                        eval_stmt w (Assign (p, Const v))
                    | _ -> raise (PascalInterp RunTimeError)
                  in
                  helper fptl ptl w
              | _ :: fptl, _ :: ptl -> helper fptl ptl w
              | [], [] -> w
              | _ -> raise (PascalInterp RunTimeError)
            in
            let w = helper fp p w in
            match load fw n with
            | _, VVariable v -> (w, v)
            | _ -> raise (PascalInterp RunTimeError))
        | v -> raise (PascalInterp (CantCall v)))
    | GetRec (r, n) ->
        let w, r = eval_expr w r in
        (w, get_rec r n)
    | GetArr (a, i) ->
        let w, a = eval_expr w a in
        let w, i = eval_expr w i in
        (w, get_arr a i)
  in
  match eval_expr_in w e with
  | _, VCollable n -> raise (PascalInterp (InvalidCall n))
  | ok -> ok

and eval_stmt (w : world) : statement -> world =
  let rec loop w e f st =
    let w, b = eval_expr w e in
    match b with
    | VBool b ->
        if f b then
          let w =
            try eval_stmt_list w st with
            | BreakEx w -> w
            | ContinueEx w -> loop w e f st
          in
          loop w e f st
        else w
    | _ -> raise (PascalInterp (InvalidType (VTBool, get_type_val b)))
  in
  let iter v step =
    match v with
    | VInt i -> VInt (i + step)
    | VChar c -> VChar (Char.chr (Char.code c + step))
    | VBool b -> (
        match Bool.to_int b + step with
        | 0 -> VBool false
        | 1 -> VBool true
        | _ -> raise (PascalInterp RunTimeError))
    | _ -> raise (PascalInterp RunTimeError)
  in
  function
  | Assign (Variable n, e) -> (
      match load w n with
      | t, VVariable _ ->
          let w, v = eval_expr w e in
          KeyMap.add n (t, VVariable v) w
      | _ -> raise (PascalInterp (NotAVariable n)))
  | Assign (l, r) ->
      let rec helper w f =
        let arr_add a i v =
          match a with
          | VArray (s, l, t, a) ->
              VArray (s, l, t, ImArray.set a (iter_arr s i) v)
          | _ -> raise (PascalInterp RunTimeError)
        in
        let rec_add r n v =
          match r with
          | VRecord w -> (
              match load w n with
              | t, VVariable _ -> VRecord (KeyMap.add n (t, VVariable v) w)
              | _ -> raise (PascalInterp RunTimeError))
          | _ -> raise (PascalInterp RunTimeError)
        in
        function
        | GetArr (e, i) ->
            let w, i = eval_expr w i in
            helper w (fun a v -> arr_add a i (f (get_arr a i) v)) e
        | GetRec (e, n) ->
            helper w (fun a v -> rec_add a n (f (get_rec a n) v)) e
        | Variable n -> (
            match load w n with
            | t, VVariable v ->
                let w, e = eval_expr w r in
                KeyMap.add n (t, VVariable (f v e)) w
            | _ -> raise (PascalInterp RunTimeError))
        | _ -> raise (PascalInterp RunTimeError)
      in
      helper w (fun _ v -> v) l
  | ProcCall e ->
      let w, _ = eval_expr w e in
      w
  | If (e, ts, es) -> (
      let w, b = eval_expr w e in
      match b with
      | VBool b -> if b then eval_stmt_list w ts else eval_stmt_list w es
      | _ -> raise (PascalInterp (InvalidType (VTBool, get_type_val b))))
  | While (e, st) -> loop w e (fun b -> b) st
  | Repeat (e, st) -> loop w e (fun b -> not b) st
  | For (n, s, f, st) ->
      let w, s = eval_expr w s in
      let w, f = eval_expr w f in
      let dir =
        match eval_binop Less s f with
        | VBool b -> if b then 1 else -1
        | _ -> raise (PascalInterp RunTimeError)
      in
      let rec helper w i =
        let update_n w =
          match load w n with
          | t, VVariable _ -> KeyMap.add n (t, VVariable i) w
          | _ -> raise (PascalInterp RunTimeError)
        in
        let w = update_n w in
        try
          let w = eval_stmt_list w st in
          let w = update_n w in
          if f = i then w else helper w (iter i dir)
        with
        | BreakEx w -> w
        | ContinueEx w -> helper w (iter i dir)
      in
      helper w s
  | Break -> raise (BreakEx w)
  | Continue -> raise (ContinueEx w)

and eval_stmt_list w = List.fold_left (fun w st -> eval_stmt w st) w

type interpret_result = Succes of world | Fail of exn

let interpret s =
  try
    match parse s with
    | None -> raise (PascalInterp ParserError)
    | Some (w, st) ->
        let w = load_variables w in
        if semantic_test w st then Succes (eval_stmt_list w st)
        else raise (PascalInterp SemanticError)
  with err -> Fail err

exception ExpFnd of name * variable * variable

let check_interp s l =
  match interpret s with
  | Succes w ->
      List.map
        (fun (n, v) ->
          load w n => fun (_, wv) ->
          if not (v = wv) then raise (ExpFnd (n, v, wv)) else true)
        l
      => fun _ -> true
  | Fail (PascalInterp err) -> raise err
  | _ -> false

let%test "simple prog" =
  check_interp
    "\n\
    \      var\n\
    \        x, y : integer;\n\
    \        function add (x, y : integer) : integer;\n\
    \        begin\n\
    \          add := x + y;\n\
    \        end;\n\
    \      begin\n\
    \        x := 32;\n\
    \        y := 10;\n\
    \        x := x + y;\n\
    \      end.\n\
    \    "
    [ ("x", VVariable (VInt 42)); ("y", VVariable (VInt 10)) ]

let%test "simple prog with call" =
  check_interp
    "\n\
    \      var\n\
    \        x, y : integer;\n\
    \        function add (x, y : integer) : integer;\n\
    \        begin\n\
    \          add := x + y;\n\
    \        end;\n\
    \      begin\n\
    \        x := 32;\n\
    \        y := 10;\n\
    \        x := add(x, y);\n\
    \      end.\n\
    \    "
    [ ("x", VVariable (VInt 42)); ("y", VVariable (VInt 10)) ]

let%test "simple prog with call out" =
  check_interp
    "\n\
    \      var\n\
    \        x, y : integer;\n\
    \        function add (out x : integer; y : integer) : integer;\n\
    \        begin\n\
    \          x := x + y;\n\
    \        end;\n\
    \      begin\n\
    \        x := 32;\n\
    \        y := 10;\n\
    \        add(x, y);\n\
    \      end.\n\
    \    "
    [ ("x", VVariable (VInt 42)); ("y", VVariable (VInt 10)) ]

let%test "array rec" =
  check_interp
    "\n\
    \  var\n\
    \    x, y, z : integer;\n\
    \    a : array [0..1, 'a'..'b'] of record\n\
    \          f : integer;\n\
    \        end;\n\
    \  begin\n\
    \    a[0, 'a'].f := 42;\n\
    \    a[1]['b'].f := 41;\n\
    \    a[1]['a'].f := 1;\n\
    \        x := a[0]['a'].f;\n\
    \    y := a[1, 'b'].f;\n\
    \    z := a[1]['a'].f;\n\
    \  end.\n\
    \  "
    [
      ("x", VVariable (VInt 42));
      ("y", VVariable (VInt 41));
      ("z", VVariable (VInt 1));
    ]

let%test "func" =
  check_interp
    "\n\
    \  var \n\
    \  x : integer;\n\
    \  function f : integer;\n\
    \  begin\n\
    \    f := 42;\n\
    \  end;\n\
    \  begin\n\
    \    x := f();\n\
    \  end.\n"
    [ ("x", VVariable (VInt 42)) ]

let%test "func as arg" =
  check_interp
    "\n\
    \    type\n\
    \      intf = function : integer;\n\
    \    var\n\
    \      x : integer;\n\
    \      f : intf;\n\
    \      function some_f : integer;\n\
    \      begin\n\
    \        some_f := 42;\n\
    \      end;\n\
    \    begin\n\
    \      f := some_f;\n\
    \      x := f();\n\
    \    end.\n\
    \  "
    [ ("x", VVariable (VInt 42)) ]

let%test "for loop" =
  check_interp
    "\n\
    \    var\n\
    \      i : integer;\n\
    \      x : integer;\n\
    \    begin\n\
    \      x := 0;\n\
    \      for i := 0 to 10 do x := x + 1;\n\
    \    end.\n\
    \  "
    [ ("x", VVariable (VInt 11)); ("i", VVariable (VInt 10)) ]

let%test "for loop" =
  check_interp
    "\n\
    \    var\n\
    \      i : integer;\n\
    \      x : integer;\n\
    \    begin\n\
    \      x := 0;\n\
    \      for i := 0 to 10 do x := i;\n\
    \    end.\n\
    \  "
    [ ("x", VVariable (VInt 10)); ("i", VVariable (VInt 10)) ]

let%test "for loop if _ then break" =
  check_interp
    "\n\
    \    var\n\
    \      i : integer;\n\
    \      x : integer;\n\
    \    begin\n\
    \      x := 0;\n\
    \      for i := 0 to 10 do if x = 5 then break else x := i;\n\
    \    end.\n\
    \  "
    [ ("x", VVariable (VInt 5)); ("i", VVariable (VInt 6)) ]

let%test "fibonacci" =
  check_interp
    "\n\
    \  var\n\
    \    a, b, c, i, n, x: integer;\n\n\
    \  begin \n\
    \    a := 0;\n\
    \    b := 1;\n\
    \    n := 10;\n\n\
    \    for i := 3 to n do begin\n\
    \        x := a + b;\n\
    \        c := b;\n\
    \        b := a + b;\n\
    \        a := c;\n\
    \        i := 42;\n\
    \    end;\n\
    \  end.\n\
    \  "
    [ ("x", VVariable (VInt 34)); ("i", VVariable (VInt 10)) ]
