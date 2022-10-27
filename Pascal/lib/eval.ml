(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open KeyMap
open Exceptions
open VTypeBasics
open ImArray

let rec eval_binop op v1 v2 =
  let ( <|> ) f1 f2 arg =
    try f1 arg with
    | _ -> f2 arg
  in
  let ( => ) arg f = f arg in
  let error = PascalInterp (BinOpTypeError (op, get_type_val v1, get_type_val v2)) in
  let v1, v2 =
    let evolution v =
      match v with
      | VInt v -> VFloat (Float.of_int v)
      | VChar v -> VString (String.make 1 v)
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
       (match op with
       | Add -> x + y
       | Sub -> x - y
       | Mul -> x * x
       | Div -> x / y
       | Mod -> x mod y
       | And -> x land y
       | Or -> x lor y
       | Xor -> x lxor y
       | RShift -> Int.shift_right x y
       | LShift -> Int.shift_left x y
       | _ -> raise error)
       => fun v -> VInt v)
    <|> (fun (op, x, y) ->
          (match op with
          | Greater -> x > y
          | GreaterEq -> x >= y
          | Less -> x < y
          | LessEq -> x <= y
          | Eq -> x == y
          | NotEq -> x != y
          | _ -> raise error)
          => fun v -> VBool v)
    <|> fun (op, x, y) ->
    (match op with
    | FDiv -> Float.of_int x /. Float.of_int y
    | _ -> raise error)
    => fun v -> VFloat v)
      (op, v1, v2)
  | VFloat v1, VFloat v2 ->
    ((fun (op, x, y) ->
       (match op with
       | Add -> x +. y
       | Sub -> x -. y
       | Mul -> x *. y
       | FDiv -> x /. y
       | _ -> raise error)
       => fun v -> VFloat v)
    <|> fun (op, x, y) ->
    (match op with
    | Greater -> x > y
    | GreaterEq -> x >= y
    | Less -> x < y
    | LessEq -> x <= y
    | Eq -> x = y
    | NotEq -> x != y
    | _ -> raise error)
    => fun v -> VBool v)
      (op, v1, v2)
  | VChar v1, VChar v2 ->
    eval_binop op (VString (String.make 1 v1)) (VString (String.make 1 v2))
  | VString v1, VString v2 ->
    ((fun (op, x, y) ->
       (match op with
       | Add -> String.concat x [ ""; y ]
       | _ -> raise error)
       => fun v -> VString v)
    <|> fun (op, x, y) ->
    (match op with
    | Greater -> x > y
    | GreaterEq -> x >= y
    | Less -> x < y
    | LessEq -> x <= y
    | Eq -> x == y
    | NotEq -> x != y
    | _ -> raise error)
    => fun v -> VBool v)
      (op, v1, v2)
  | VBool v1, VBool v2 ->
    (fun (op, x, y) ->
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
      | _ -> raise error)
      => fun v -> VBool v)
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
    | VTChar, VTChar -> VTNDString
    | VTChar, VTNDString -> VTNDString
    | VTNDString, VTNDString -> VTNDString
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
    | VTChar, VTNDString -> VTBool
    | VTNDString, VTNDString -> VTBool
    | VTBool, VTBool -> VTBool
    | _ -> raise error)
;;

let eval_unop op v =
  let error = PascalInterp (UnOpTypeError (op, get_type_val v)) in
  match op with
  | Plus ->
    (match v with
    | (VInt _ | VFloat _) as ok -> ok
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
    | [ VTFloat ] -> VTInt
    | _ -> raise type_error)
  | "chr" ->
    (match pt with
    | [ VTInt ] -> VTChar
    | _ -> raise type_error)
  | "ord" ->
    (match pt with
    | [ VTChar ] -> VTChar
    | _ -> raise type_error)
  | _ -> raise error
;;

let get_rec v n =
  match v with
  | VRecord w ->
    (match KeyMap.find_opt n w with
    | Some (_, VConst v) | Some (_, VVariable v) -> v
    | _ -> raise (PascalInterp (RecordFieldError (get_type_val v, n))))
  | _ -> raise (PascalInterp (RecordTypeError (get_type_val v)))
;;

let%test "get_rec test" =
  get_rec
    (VRecord (KeyMap.of_seq (List.to_seq [ "i", (VTInt, VVariable (VInt 42)) ])))
    "i"
  = VInt 42
;;

let get_rec_type t n =
  match t with
  | VTDRecord w ->
    (match KeyMap.find_opt n w with
    | Some t -> t
    | _ -> raise (PascalInterp (RecordFieldError (t, n))))
  | _ -> raise (PascalInterp (RecordTypeError t))
;;

let iter_arr s f =
  match s, f with
  | VInt s, VInt f -> f - s
  | VChar s, VChar f -> Char.code f - Char.code s
  | VBool s, VBool f -> Bool.to_int f - Bool.to_int s
  | _ -> raise (PascalInterp TypeError)
;;

let%test "iter arr int" = iter_arr (VInt 10) (VInt 32) = 22
let%test "iter arr bool" = iter_arr (VBool false) (VBool true) = 1

let get_arr v ind =
  match v with
  | VArray (start, size, _, arr) ->
    let act_ind = iter_arr start ind in
    if act_ind < 0 || act_ind >= size
    then raise (PascalInterp (ArrayOutOfInd (get_type_val v, ind)))
    else ImArray.get arr act_ind
  | _ -> raise (PascalInterp (ArrayTypeError (get_type_val v)))
;;

let%test "get arr" =
  get_arr
    (VArray (VChar 'a', 3, VTInt, ImArray.of_list [ VInt 1; VInt 2; VInt 3 ]))
    (VChar 'c')
  = VInt 3
;;

let get_arr_type t ind =
  match t with
  | VTDArray (start, _, t) when compare_types (get_type_val start) ind -> t
  | _ -> raise (PascalInterp (ArrayTypeError t))
;;

let rec eval_expr_base load eval_function world =
  let eval_expr e = eval_expr_base load eval_function world e in
  let eval_function = eval_function world in
  let load = load world in
  function
  | Const v -> v
  | Variable n -> load n
  | BinOp (op, x, y) -> eval_binop op (eval_expr x) (eval_expr y)
  | UnOp (op, x) -> eval_unop op (eval_expr x)
  | Call (e, p) -> eval_function (eval_expr e) p
  | GetRec (r, n) -> get_rec (eval_expr r) n
  | GetArr (a, i) -> get_arr (eval_expr a) (eval_expr i)
;;

let rec eval_expr_base_type load_type eval_function_type world =
  let eval_expr e = eval_expr_base_type load_type eval_function_type world e in
  let eval_function = eval_function_type world in
  let load = load_type world in
  function
  | Const v -> get_type_val v
  | Variable n -> load n
  | BinOp (op, x, y) -> eval_binop_type op (eval_expr x) (eval_expr y)
  | UnOp (op, x) -> eval_unop_type op (eval_expr x)
  | Call (e, p) -> eval_function (eval_expr e) p
  | GetRec (r, n) -> get_rec_type (eval_expr r) n
  | GetArr (a, i) -> get_arr_type (eval_expr a) (eval_expr i)
;;

let rec eval_expr_const w e =
  let const_eval_function w f p =
    match f with
    | VCollable n -> eval_std_function n (List.map (eval_expr_const w) p)
    | _ -> raise (PascalInterp (CantCall f))
  in
  let const_loader w n =
    match KeyMap.find_opt n w with
    | Some (VTFunction _, _) -> raise (PascalInterp (NotAConst n))
    | Some (_, VConst v) -> v
    | None -> VCollable n
    | _ -> raise (PascalInterp (NotAConst n))
  in
  (eval_expr_base const_loader const_eval_function) w e
;;

let%test "eval expr partal" = eval_expr_const KeyMap.empty (Const (VInt 42)) = VInt 42

let%test "eval expr 1 + 1" =
  eval_expr_const KeyMap.empty (BinOp (Add, Const (VInt 1), Const (VInt 1))) = VInt 2
;;

let%test "eval expr const variables" =
  eval_expr_const
    (KeyMap.of_seq
       (List.to_seq [ "x", (VTInt, VConst (VInt 32)); "y", (VTInt, VConst (VInt 10)) ]))
    (BinOp (Add, Variable "x", Variable "y"))
  = VInt 42
;;

let%test "eval expr const round" =
  eval_expr_const KeyMap.empty (Call (Variable "round", [ Const (VFloat 3.) ])) = VInt 3
;;
