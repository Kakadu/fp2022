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
     | VTVoid, _ -> false
     | _, (VVariable _ | VFunctionResult _) -> true
     | _, (VConst _ | VType) -> false)
  | _ -> false
;;

let rec eval_expr_type e w =
  let eval_expr e = eval_expr_type e w in
  let loader n w =
    match Worlds.load n w with
    | t, (VVariable _ | VConst _ | VFunctionResult _) -> t
    | _, VType -> raise (PascalInterp (NotAVariable n))
  in
  let eval_function f p w =
    let compare_param_list =
      List.for_all2 (fun p ep ->
        let tep = eval_expr ep in
        match p with
        | FPFree (_, tp) | FPConst (_, tp) -> cast_type tp tep
        | FPOut (_, tp) -> is_l_value ep w && cast_type tp tep)
    in
    match f with
    | Variable fn ->
      (match Worlds.load_fun_opt fn w with
       | Some ((VTFunction (fpl, t) | VTConstFunction (fpl, t)), _)
         when compare_param_list fpl p -> return t w
       | None ->
         let p = List.map (fun e -> eval_expr e) p in
         return (eval_std_function_type fn p) w
       | _ -> raise (PascalInterp (InvalidCall fn)))
    | _ -> raise (PascalInterp TypeError)
  in
  use (eval_expr_base_type loader eval_function e) w
;;

let rec eval_stmt_type ?(func = false) ?(loop = false) s w =
  let eval_expr e = eval_expr_type e w in
  let eval_stmt_list sl = eval_stmt_list_type ~func ~loop sl w in
  let eval_stmt_list_loop sl = eval_stmt_list_type ~func ~loop:true sl w in
  match s with
  | Assign (l, r) when is_l_value l w ->
    let l = eval_expr l in
    let r = eval_expr r in
    if not (cast_type l r) then raise (PascalInterp (InvalidType (l, r)))
  | AssignFunc (l, r) when is_l_value l w ->
    let l = eval_expr l in
    let r = Worlds.load_const_fun r w |> fun (t, _) -> t in
    if not (compare_types l r) then raise (PascalInterp (InvalidType (l, r)))
  | Assign (l, _) | AssignFunc (l, _) -> raise (PascalInterp (LeftValError l))
  | ProcCall (f, pl) -> eval_expr (Call (f, pl)) |> fun _ -> ()
  | If (b, t, e) ->
    let b = eval_expr b in
    if not (cast_type VTBool b)
    then raise (PascalInterp (InvalidType (VTBool, b)))
    else eval_stmt_list t |> fun _ -> eval_stmt_list e
  | While (b, t) | Repeat (b, t) ->
    let b = eval_expr b in
    if not (cast_type VTBool b)
    then raise (PascalInterp (InvalidType (VTBool, b)))
    else eval_stmt_list_loop t
  | For (n, s, f, t) ->
    let nt, _ = Worlds.load n w in
    let s = eval_expr s in
    let f = eval_expr f in
    if not (is_iterable s)
    then raise (PascalInterp (NotIterable s))
    else if not (compare_types f s)
    then raise (PascalInterp (InvalidType (s, f)))
    else if not (compare_types nt s)
    then raise (PascalInterp (InvalidType (s, nt)))
    else eval_stmt_list_loop t
  | Break | Continue -> if not loop then raise (PascalInterp NotInALoop)
  | Exit -> if not func then raise (PascalInterp NotInAFunction)

and eval_stmt_list_type ?(func = false) ?(loop = false) sl w =
  List.for_all (fun s -> eval_stmt_type ~func ~loop s w |> fun _ -> true) sl
  |> fun _ -> ()
;;

let with_semantic_test interp stmt world =
  let rec semantic_test_in func c = function
    | h :: _ as w ->
      if KeyMap.for_all
           (fun _ -> function
             | VTConstFunction _, VConst (VFunction (_, _, _, fw, fc)) ->
               semantic_test_in true fc (fw :: w) |> fun _ -> true
             | VTConstFunction _, _ -> raise (PascalInterp RunTimeError)
             | _ -> true)
           h
      then eval_stmt_list_type ~func c w
    | _ -> raise (PascalInterp RunTimeError)
  in
  semantic_test_in false stmt world |> fun _ -> interp stmt world
;;

exception BreakEx of Worlds.t
exception ContinueEx of Worlds.t
exception ExitEx of Worlds.t

let rec eval_expr e w =
  let loader n w =
    let _, v = Worlds.load n w in
    match v with
    | VVariable v | VConst v | VFunctionResult v -> v
    | VType -> raise (PascalInterp (NotAVariable n))
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
      | _, VType -> raise (PascalInterp (NotAVariable n))
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
      (match Worlds.load_fun_all_opt f w with
       | Some (wh, (_, VFunction (n, _, fp, fw, st)), wtl) ->
         let r, wtl = eval_function n fp pexp p fw st wtl in
         r, List.rev_append wh wtl
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
    | _ -> raise (PascalInterp (InvalidType (VTBool, get_type_val b)))
  in
  let assign l eval r w =
    let rec helper w f =
      let arr_add a i v =
        match a with
        | VArray (s, l, t, a) -> VArray (s, l, t, ImArray.set a (iter_arr s i) v)
        | VString (s, _) ->
          (match i, v with
           | VInt i, VChar v ->
             let s = String.mapi (fun ci c -> if ci = i then v else c) s in
             VString (s, String.length s)
           | _, VChar _ -> raise (PascalInterp (InvalidType (VTInt, get_type_val i)))
           | _, _ -> raise (PascalInterp (InvalidType (VTChar, get_type_val v))))
        | _ -> raise (PascalInterp (NotIterable (get_type_val a)))
      in
      let rec_add r n v =
        match r with
        | VRecord w ->
          (match Worlds.load n [ w ] with
           | t, VVariable _ -> VRecord (KeyMap.add n (t, VVariable v) w)
           | _ -> raise (PascalInterp RunTimeError))
        | _ -> raise (PascalInterp (InvalidType (VTRecord KeyMap.empty, get_type_val r)))
      in
      function
      | GetArr (e, i) ->
        let i, w = eval_expr i w in
        helper w (fun a v -> arr_add a i (f (get_arr i a) v)) e
      | GetRec (e, n) -> helper w (fun a v -> rec_add a n (f (get_rec n a) v)) e
      | Variable l ->
        (match Worlds.load l w with
         | t, (VVariable v | VFunctionResult v) ->
           let nv, w = eval r w in
           Worlds.replace l (t, f v nv) w
         | _, (VConst _ | VType) -> raise (PascalInterp (NotAVariable l)))
      | e -> raise (PascalInterp (LeftValError e))
    in
    helper w (fun _ v -> v) l
  in
  match s with
  | Assign (l, r) -> assign l eval_expr r w
  | AssignFunc (l, r) ->
    assign l (fun f w -> Worlds.load_const_fun f w |> fun (_, v) -> v, w) r w
  | ProcCall (e, pl) -> eval_expr (Call (e, pl)) w |> fun (_, w) -> w
  | If (e, ts, es) ->
    let b, w = eval_expr e w in
    (match b with
     | VBool b -> eval_stmt_list (if b then ts else es) w
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
        | t, (VVariable _ | VFunctionResult _) -> Worlds.replace n (t, i) w
        | _ -> raise (PascalInterp (NotAVariable n))
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
    (match with_semantic_test eval_stmt_list st w with
     | w :: [] -> w
     | _ -> raise (PascalInterp RunTimeError))
;;

let interpret s =
  try Ok (interpret_no_catch s) with
  | err -> Error err
;;

exception ExpFnd of name * variable * variable

let check_interp s l =
  try
    let w = interpret_no_catch s in
    List.map
      (fun (n, v) ->
        Worlds.load n [ w ]
        |> function
        | _, wv when not (v = wv) -> raise (ExpFnd (n, v, wv))
        | _ -> true)
      l
    |> fun _ -> true
  with
  | ExpFnd (n, v1, v2) ->
    Printf.printf "%s : %s | %s\n\n" n (show_variable v1) (show_variable v2);
    false
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

let%test "special assigns" =
  check_interp
    {|
      var
        f : real;
        s : string[3];
        a : array[1..5] of char;
        h : string;
      begin
        f := 2;
        s := 'c';
        a := 'hello world, i am the string!';
        h := a;
      end.
    |}
    [ "f", VVariable (VFloat (Float.of_int 2))
    ; "s", VVariable (VString ("c", 3))
    ; "h", VVariable (VString ("hello", 255))
    ]
;;

let%test "but we can not assigns" =
  try
    check_interp
      {|
        var
          i : integer;
        begin
          i := 1.;
        end.
      |}
      [ "i", VVariable (VInt 1) ]
    |> fun _ -> false
  with
  | PascalInterp (InvalidType (VTInt, VTFloat)) -> true
;;

let%test "type casting" =
  check_interp
    {|
      var
        x, y : integer;
        r : real;
        function add (x, y : real) : real;
        begin
          add := x + y;
        end;
      begin
        x := 32;
        y := 10;
        r := add(x, y);
      end.
    |}
    [ "r", VVariable (VFloat (Float.of_int 32 +. Float.of_int 10)) ]
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

let%test "array overflow" =
  try
    check_interp
      {|
        var
          x, y, z : integer;
          a : array [0..1] of integer;
        begin
          a[42] := 42;
          x := 0;
        end.
      |}
      [ "x", VVariable (VInt 0) ]
    |> fun _ -> false
  with
  | PascalInterp (ArrayOutOfInd (VTArray (VInt 0, 2, VTInt), VInt 42)) -> true
;;

let%test "string add char" =
  check_interp
    {|
      var
        s : string;
        c : char;
      begin
        s := 'str';
        c := 'n';
        s := c + s;
      end.
    |}
    [ "s", VVariable (VString ("nstr", 255)) ]
;;

let%test "string overflow" =
  check_interp
    {|
      var
        s : string[2];
      begin
        s := 'str';
      end.
    |}
    [ "s", VVariable (VString ("st", 2)) ]
;;

let%test "string not overflow" =
  check_interp
    {|
      var
        s : string[2];
      begin
        s := 'st';
      end.
    |}
    [ "s", VVariable (VString ("st", 2)) ]
;;

let%test "string overflow" =
  check_interp
    {|
      var
        s : string[2] = 'abc';
      begin
      end.
    |}
    [ "s", VVariable (VString ("ab", 2)) ]
;;

let%test "string not overflow" =
  check_interp
    {|
      var
        s : string[2] = 'ab';
      begin
      end.
    |}
    [ "s", VVariable (VString ("ab", 2)) ]
;;

let%test "string sum" =
  check_interp
    {|
      var
        s : string;
      begin
        s := 'hello' + ' ' + 'world!';
      end.
    |}
    [ "s", VVariable (VString ("hello world!", 255)) ]
;;

let%test "string as array" =
  check_interp
    {|
      var
        s : string = '0123456';
        c1, c2 : char;
      begin
        c1 := s[3];
        s[3] := 'x';
        c2 := s[3];
      end.
    |}
    [ "c1", VVariable (VChar '3')
    ; "c2", VVariable (VChar 'x')
    ; "s", VVariable (VString ("012x456", 255))
    ]
;;

let%test "string as array and length fun" =
  check_interp
    {|
      const n = 5;
      var
        s : string;
      begin
        if length('hello world') <= n then s := 'hello world'
        else if length('x') <= n then s := 'x'
        else s := '!';
      end.
    |}
    [ "s", VVariable (VString ("x", 255)) ]
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

let%test "func exit" =
  check_interp
    {|
      var
        x : integer;
        function f : integer;
        begin
          for f := 0 to 50 do
            if f = 42 then exit;
        end;
      begin
        x := f();
      end.
    |}
    [ "x", VVariable (VInt 42) ]
;;

let%test "func as arg wrong using" =
  try
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
    |> fun _ -> false
  with
  | PascalInterp (InvalidType _) -> true
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
        f := @some_f;
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
        f := ((@some_f));
        x := f();
      end.
    |}
    [ "x", VVariable (VInt 42) ]
;;

let%test "func as arg wrong using" =
  try
    check_interp
      {|
        type
          int_f = function : integer;
        var
          x : integer;
          f, g : int_f;
          function some_f : integer;
          begin
            some_f := 42;
          end;
        begin
          f := @some_f;
          g := @f;
          x := g();
        end.
      |}
      [ "x", VVariable (VInt 42) ]
  with
  | PascalInterp (VariableNotFound "f") -> true
;;

let%test "func as arg" =
  check_interp
    {|
      type
        int_f = function : integer;
      var
        x : integer;
        f, g : int_f;
        function some_f : integer;
        begin
          some_f := 42;
        end;
      begin
        f := @some_f;
        g := f;
        x := g();
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
        f, g : int_f;
        function some_f : integer;
        begin
          f := @some_f;
          some_f := 42;
        end;
      begin
        some_f();
        g := f;
        x := g();
      end.
    |}
    [ "x", VVariable (VInt 42) ]
;;

let%test "proc" =
  check_interp
    {|
      type
        int_f = function : integer;
      var
        x : integer;
        f, g : int_f;
        procedure some_f (i : integer);
        begin
          x := i;
        end;
      begin
        some_f(42);
      end.
    |}
    [ "x", VVariable (VInt 42) ]
;;

let%test "proc" =
  try
    check_interp
      {|
        type
          int_f = function : integer;
        var
          x : integer;
          f, g : int_f;
          procedure p;
          begin
          end;
          procedure some_f (i : integer);
          begin
            some_f := p();
            x := i;
          end;
        begin
          some_f(42);
        end.
      |}
      [ "x", VVariable (VInt 42) ]
    |> fun _ -> false
  with
  | PascalInterp (LeftValError _) -> true
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
        for i := 0 to 10 do
          begin
            x := i;
            i := 42;
          end;
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
      const n = 10;
      var
        a, b, i, x : integer;
      begin
        a := 0;
        b := 1;
        for i := 2 to n do begin
          b := a + b;
          a := b - a;
        end;
        x := b;
      end.
    |}
    [ "x", VVariable (VInt 55) ]
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
            f := 0
          else if n = 1 then
            f := 1
          else
            f := f(n - 1) + f(n - 2);
        end;
      begin
        i := f(n);
      end.
    |}
    [ "i", VVariable (VInt 55) ]
;;

let%test "factorial rec" =
  check_interp
    {|
      const n = 6;
      var
        i : integer;
        function fac (n : integer) : integer;
        begin
          if n = 0 then
            fac := 1
          else
            fac := n * fac(n - 1);
        end;
      begin
        i := fac(n);
      end.
    |}
    [ "i", VVariable (VInt 720) ]
;;
