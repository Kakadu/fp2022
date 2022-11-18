(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Exceptions
open VTypeBasics
open Monadic

let rec eval_binop op v1 v2 =
  let ( <|> ) f1 f2 arg =
    try f1 arg with
    | PascalInterp _ -> f2 arg
    | err -> raise err
  in
  let error = PascalInterp (BinOpTypeError (op, get_type_val v1, get_type_val v2)) in
  let v1, v2 =
    let evolution v =
      match v with
      | VInt v -> VFloat (Float.of_int v)
      | VChar v -> VString (String.make 1 v, 1)
      | v -> v
    in
    if compare_types (get_type_val v1) (get_type_val v2)
    then v1, v2
    else (
      let v1 = evolution v1 in
      let v2 = evolution v2 in
      if compare_types (get_type_val v1) (get_type_val v2) then v1, v2 else raise error)
  in
  match v1, v2 with
  | VInt v1, VInt v2 ->
    ((fun (op, x, y) ->
       VInt
         (match op with
          | Add -> x + y
          | Sub -> x - y
          | Mul -> x * y
          | Div -> x / y
          | Mod -> x mod y
          | And -> x land y
          | Or -> x lor y
          | Xor -> x lxor y
          | RShift -> Int.shift_right x y
          | LShift -> Int.shift_left x y
          | _ -> raise error))
    <|> (fun (op, x, y) ->
          VBool
            (match op with
             | Greater -> x > y
             | GreaterEq -> x >= y
             | Less -> x < y
             | LessEq -> x <= y
             | Eq -> x == y
             | NotEq -> x != y
             | _ -> raise error))
    <|> fun (op, x, y) ->
    VFloat
      (match op with
       | FDiv -> Float.of_int x /. Float.of_int y
       | _ -> raise error))
      (op, v1, v2)
  | VFloat v1, VFloat v2 ->
    ((fun (op, x, y) ->
       VFloat
         (match op with
          | Add -> x +. y
          | Sub -> x -. y
          | Mul -> x *. y
          | FDiv -> x /. y
          | _ -> raise error))
    <|> fun (op, x, y) ->
    VBool
      (match op with
       | Greater -> x > y
       | GreaterEq -> x >= y
       | Less -> x < y
       | LessEq -> x <= y
       | Eq -> x = y
       | NotEq -> x != y
       | _ -> raise error))
      (op, v1, v2)
  | VChar v1, VChar v2 ->
    eval_binop op (VString (String.make 1 v1, 1)) (VString (String.make 1 v2, 1))
  | VString (v1, _), VString (v2, _) ->
    ((fun (op, x, y) ->
       let s =
         match op with
         | Add -> x ^ y
         | _ -> raise error
       in
       VString (s, String.length s))
    <|> fun (op, x, y) ->
    VBool
      (match op with
       | Greater -> x > y
       | GreaterEq -> x >= y
       | Less -> x < y
       | LessEq -> x <= y
       | Eq -> x == y
       | NotEq -> x != y
       | _ -> raise error))
      (op, v1, v2)
  | VBool v1, VBool v2 ->
    (fun (op, x, y) ->
      VBool
        (match op with
         | And -> x && y
         | Or -> x || y
         | Xor -> x || (y && (not x) && y)
         | Greater -> x > y
         | GreaterEq -> x >= y
         | Less -> x < y
         | LessEq -> x <= y
         | Eq -> x == y
         | NotEq -> x != y
         | _ -> raise error))
      (op, v1, v2)
  | _ -> raise error
;;

let%test "eval binop add" = eval_binop Add (VInt 10) (VInt 32) = VInt 42
let%test "eval binop and" = eval_binop And (VBool true) (VBool false) = VBool false
let%test "eval binop less" = eval_binop Less (VInt 1) (VInt 42) = VBool true

let eval_binop_type op t1 t2 =
  let error = PascalInterp (BinOpTypeError (op, t1, t2)) in
  let types =
    match t2 with
    | (VTInt | VTChar) as t2 -> t2, t1
    | t2 -> t1, t2
  in
  match op with
  | Add ->
    (match types with
     | VTInt, VTInt -> VTInt
     | VTInt, VTFloat -> VTFloat
     | VTFloat, VTFloat -> VTFloat
     | VTChar, VTChar -> VTString 2
     | VTChar, VTString s -> VTString (s + 1)
     | VTString s1, VTString s2 -> VTString (s1 + s2)
     | _ -> raise error)
  | Sub | Mul ->
    (match types with
     | VTInt, VTInt -> VTInt
     | VTInt, VTFloat -> VTFloat
     | VTFloat, VTFloat -> VTFloat
     | _ -> raise error)
  | FDiv ->
    (match types with
     | VTInt, VTInt -> VTFloat
     | VTInt, VTFloat -> VTFloat
     | VTFloat, VTFloat -> VTFloat
     | _ -> raise error)
  | Div | Mod | RShift | LShift ->
    (match types with
     | VTInt, VTInt -> VTInt
     | _ -> raise error)
  | And | Or | Xor ->
    (match types with
     | VTInt, VTInt -> VTInt
     | VTBool, VTBool -> VTBool
     | _ -> raise error)
  | Greater | Less | Eq | NotEq | GreaterEq | LessEq ->
    (match types with
     | VTInt, VTInt -> VTBool
     | VTInt, VTFloat -> VTBool
     | VTFloat, VTFloat -> VTBool
     | VTChar, VTChar -> VTBool
     | VTChar, VTString _ -> VTBool
     | VTString _, VTString _ -> VTBool
     | VTBool, VTBool -> VTBool
     | _ -> raise error)
;;

let eval_unop op v =
  let error = PascalInterp (UnOpTypeError (op, get_type_val v)) in
  match op with
  | Plus ->
    (match v with
     | VInt _ | VFloat _ -> v
     | _ -> raise error)
  | Minus ->
    (match v with
     | VInt v -> VInt (-v)
     | VFloat v -> VFloat (-.v)
     | _ -> raise error)
  | Not ->
    (match v with
     | VBool v -> VBool (not v)
     | _ -> raise error)
;;

let eval_unop_type op t =
  let error = PascalInterp (UnOpTypeError (op, t)) in
  match op with
  | Plus ->
    (match t with
     | VTInt -> VTInt
     | VTFloat -> VTFloat
     | _ -> raise error)
  | Minus ->
    (match t with
     | VTInt -> VTInt
     | VTFloat -> VTFloat
     | _ -> raise error)
  | Not ->
    (match t with
     | VTBool -> VTBool
     | _ -> raise error)
;;

let eval_std_function n p =
  let error = PascalInterp (NotAStdFunction n) in
  let type_error = PascalInterp (StdFunctionTypeError (n, List.map get_type_val p)) in
  match n with
  | "abs" ->
    (match p with
     | [ VInt v ] -> VInt (abs v)
     | [ VFloat v ] -> VFloat (abs_float v)
     | _ -> raise type_error)
  | "arctan" ->
    (match p with
     | [ VInt v ] -> VFloat (atan (Float.of_int v))
     | [ VFloat v ] -> VFloat (atan v)
     | _ -> raise type_error)
  | "cos" ->
    (match p with
     | [ VInt v ] -> VFloat (cos (Float.of_int v))
     | [ VFloat v ] -> VFloat (cos v)
     | _ -> raise type_error)
  | "exp" ->
    (match p with
     | [ VInt v ] -> VFloat (exp (Float.of_int v))
     | [ VFloat v ] -> VFloat (exp v)
     | _ -> raise type_error)
  | "ln" ->
    (match p with
     | [ VInt v ] -> VFloat (log (Float.of_int v))
     | [ VFloat v ] -> VFloat (log v)
     | _ -> raise type_error)
  | "round" ->
    (match p with
     | [ (VInt _ as v) ] -> v
     | [ VFloat v ] -> VInt (Float.to_int (Float.round v))
     | _ -> raise type_error)
  | "sin" ->
    (match p with
     | [ VInt v ] -> VFloat (sin (Float.of_int v))
     | [ VFloat v ] -> VFloat (sin v)
     | _ -> raise type_error)
  | "sqr" ->
    (match p with
     | [ VInt v ] -> VInt (v * v)
     | [ VFloat v ] -> VFloat (v *. v)
     | _ -> raise type_error)
  | "sqrt" ->
    (match p with
     | [ VInt v ] -> VFloat (Float.sqrt (Float.of_int v))
     | [ VFloat v ] -> VFloat (Float.sqrt v)
     | _ -> raise type_error)
  | "trunc" ->
    (match p with
     | [ (VInt _ as v) ] -> v
     | [ VFloat v ] -> VInt (Float.to_int (Float.trunc v))
     | _ -> raise type_error)
  | "chr" ->
    (match p with
     | [ VInt v ] -> VChar (Char.chr v)
     | _ -> raise type_error)
  | "ord" ->
    (match p with
     | [ VChar v ] -> VInt (Char.code v)
     | _ -> raise type_error)
  | "length" ->
    (match p with
     | [ VChar _ ] -> VInt 1
     | [ VString (_, i) ] -> VInt i
     | [ VArray (_, s, _, _) ] -> VInt s
     | _ -> raise type_error)
  | _ -> raise error
;;

let eval_std_function_type n pt =
  let error = PascalInterp (NotAStdFunction n) in
  let type_error = PascalInterp (StdFunctionTypeError (n, pt)) in
  match n with
  | "abs" | "sqr" | "exp" | "ln" | "sin" | "sqrt" ->
    (match pt with
     | [ VTInt ] -> VTInt
     | [ VTFloat ] -> VTFloat
     | _ -> raise type_error)
  | "arctan" | "cos" ->
    (match pt with
     | [ VTInt ] -> VTFloat
     | [ VTFloat ] -> VTFloat
     | _ -> raise type_error)
  | "round" | "trunc" ->
    (match pt with
     | [ VTInt ] | [ VTFloat ] -> VTInt
     | _ -> raise type_error)
  | "chr" ->
    (match pt with
     | [ VTInt ] -> VTChar
     | _ -> raise type_error)
  | "ord" ->
    (match pt with
     | [ VTChar ] -> VTChar
     | _ -> raise type_error)
  | "length" ->
    (match pt with
     | [ VTChar ] -> VTInt
     | [ VTString _ ] -> VTInt
     | [ VTArray _ ] -> VTInt
     | _ -> raise type_error)
  | _ -> raise error
;;

let get_rec n = function
  | VRecord w as v ->
    (match KeyMap.find_opt n w with
     | Some (_, (VConst v | VVariable v | VFunctionResult v)) -> v
     | Some (_, VType) | None ->
       raise (PascalInterp (RecordFieldError (get_type_val v, n))))
  | v -> raise (PascalInterp (RecordTypeError (get_type_val v)))
;;

let%test "get_rec test" =
  get_rec
    "i"
    (VRecord (KeyMap.of_seq (List.to_seq [ "i", (VTInt, VVariable (VInt 42)) ])))
  = VInt 42
;;

let get_rec_type n = function
  | VTRecord w as t ->
    (match KeyMap.find_opt n w with
     | Some t -> t
     | None -> raise (PascalInterp (RecordFieldError (t, n))))
  | t -> raise (PascalInterp (RecordTypeError t))
;;

let iter v step =
  match v with
  | VInt i -> VInt (i + step)
  | VChar c -> VChar (Char.chr (Char.code c + step))
  | VBool b ->
    (match Bool.to_int b + step with
     | 0 -> VBool false
     | 1 -> VBool true
     | _ -> raise (PascalInterp RunTimeError))
  | _ -> raise (PascalInterp (NotIterable (get_type_val v)))
;;

let iter_arr s f =
  match s, f with
  | VInt s, VInt f -> f - s
  | VChar s, VChar f -> Char.code f - Char.code s
  | VBool s, VBool f -> Bool.to_int f - Bool.to_int s
  | _ -> raise (PascalInterp TypeError)
;;

let is_iterable = function
  | VTInt | VTChar | VTBool -> true
  | _ -> false
;;

let%test "iter arr int" = iter_arr (VInt 10) (VInt 32) = 22
let%test "iter arr bool" = iter_arr (VBool false) (VBool true) = 1

let get_arr ind = function
  | VArray (start, size, _, arr) as v ->
    let act_ind = iter_arr start ind in
    if act_ind < 0 || act_ind >= size
    then raise (PascalInterp (ArrayOutOfInd (get_type_val v, ind)))
    else ImArray.get arr act_ind
  | VString (s, sz) ->
    (match ind with
     | VInt ind when ind < sz -> VChar (String.get s ind)
     | VInt _ -> VChar (Char.chr 0)
     | _ -> raise (PascalInterp (InvalidType (VTInt, get_type_val ind))))
  | v -> raise (PascalInterp (ArrayTypeError (get_type_val v)))
;;

let%test "get arr" =
  get_arr
    (VChar 'c')
    (VArray (VChar 'a', 3, VTInt, ImArray.of_list [ VInt 1; VInt 2; VInt 3 ]))
  = VInt 3
;;

let get_arr_type ind = function
  | VTArray (start, _, t) when compare_types (get_type_val start) ind -> t
  | VTString _ when compare_types VTInt ind -> VTChar
  | t -> raise (PascalInterp (ArrayTypeError t))
;;

let rec eval_expr_base load_f eval_function =
  let eval_expr expr world = eval_expr_base load_f eval_function expr world in
  function
  | Const v -> return v
  | Variable n -> fun w -> return (load_f n w) w
  | BinOp (op, x, y) ->
    let* x = eval_expr x in
    let* y = eval_expr y in
    return (eval_binop op x y)
  | UnOp (op, x) -> eval_expr x => eval_unop op
  | Call (f, p) -> eval_function f p
  | GetRec (r, n) -> eval_expr r => get_rec n
  | GetArr (a, i) ->
    let* a = eval_expr a in
    let* i = eval_expr i in
    return (get_arr i a)
;;

let rec eval_expr_base_type load_f eval_function =
  let eval_expr expr world = eval_expr_base_type load_f eval_function expr world in
  function
  | Const v -> return (get_type_val v)
  | Variable n -> fun w -> return (load_f n w) w
  | BinOp (op, x, y) ->
    let* x = eval_expr x in
    let* y = eval_expr y in
    return (eval_binop_type op x y)
  | UnOp (op, x) -> eval_expr x => eval_unop_type op
  | Call (f, p) -> eval_function f p
  | GetRec (r, n) -> eval_expr r => get_rec_type n
  | GetArr (a, i) ->
    let* a = eval_expr a in
    let* i = eval_expr i in
    return (get_arr_type i a)
;;

let rec eval_expr_const e =
  let const_loader n w =
    match Worlds.load n w with
    | (VTFunction _ | VTConstFunction _), _ -> raise (PascalInterp (NotAConst n))
    | _, VConst v -> v
    | _ -> raise (PascalInterp (NotAConst n))
  in
  let const_eval_function f p w =
    match f with
    | Variable f ->
      let vl = List.map (fun e -> eval_expr_const e w) p in
      return (eval_std_function f vl) w
    | _ -> raise (PascalInterp NonConstCall)
  in
  use (eval_expr_base const_loader const_eval_function e)
;;

let%test "eval expr partial" = eval_expr_const (Const (VInt 42)) [] = VInt 42

let%test "eval expr 1 + 1" =
  eval_expr_const (BinOp (Add, Const (VInt 1), Const (VInt 1))) [] = VInt 2
;;

let%test "eval expr const (2 + 2) * 2" =
  eval_expr_const
    (BinOp (Mul, BinOp (Add, Const (VInt 2), Const (VInt 2)), Const (VInt 2)))
    []
  = VInt 8
;;

let%test "eval expr const variables" =
  eval_expr_const
    (BinOp (Add, Variable "x", Variable "y"))
    [ KeyMap.of_seq
        (List.to_seq [ "x", (VTInt, VConst (VInt 32)); "y", (VTInt, VConst (VInt 10)) ])
    ]
  = VInt 42
;;

let%test "eval expr const round" =
  eval_expr_const (Call (Variable "round", [ Const (VFloat 3.) ])) [] = VInt 3
;;

let%test "eval expr const custom function" =
  try
    eval_expr_const
      (Call (Variable "mf", [ Const (VBool true) ]))
      [ KeyMap.of_seq
          (List.to_seq
             [ "mf", (VTFunction ([ FPFree ("b", VTBool) ], VTBool), VConst VVoid) ])
      ]
    = VBool true
  with
  | PascalInterp (NotAStdFunction "mf") -> true
  | _ -> false
;;

let%test "eval expr const in another world" =
  eval_expr_const
    (BinOp (Add, Variable "x", Variable "y"))
    [ KeyMap.of_seq (List.to_seq [ "x", (VTInt, VConst (VInt 32)) ])
    ; KeyMap.of_seq (List.to_seq [ "y", (VTInt, VConst (VInt 10)) ])
    ]
  = VInt 42
;;
