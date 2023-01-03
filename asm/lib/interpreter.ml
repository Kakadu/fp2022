(** Copyright 2021-2022, Startsev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Int64

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >> ) : 'a t -> 'b t -> 'b t
end

module type MONADERROR = sig
  include MONAD

  val error : string -> 'a t
end

module Result = struct
  type 'a t = ('a, string) Result.t

  let ( >>= ) = Result.bind
  let ( >> ) x f = x >>= fun _ -> f
  let return = Result.ok
  let error = Result.error
end

module Interpret (M : MONADERROR) = struct
  open M

  module MapVar = struct
    include Map.Make (String)

    let pp pp_v ppf m =
      Format.fprintf ppf "@[[@[";
      iter (fun k v -> Format.fprintf ppf "@[\"%s\": %a@],@\n" k pp_v v) m;
      Format.fprintf ppf "@]]@]"
  end

  (** global constans *)
  type var =
    | Flag of bool  (** EFLAGS *)
    | Reg64 of int64  (** not so large registers *)
    | Reg128 of int64 * int64  (** large registers *)
    | Const of int64  (** global consts *)
  [@@deriving show { with_path = false }]

  type envr = var MapVar.t [@@deriving show { with_path = false }]

  (** start values of registers *)
  let r_list =
    [
      ("ZF", Flag false);
      ("SF", Flag false);
      ("OF", Flag false);
      ("RAX", Reg64 0L);
      ("RBX", Reg64 0L);
      ("RCX", Reg64 0L);
      ("RDX", Reg64 0L);
      ("RSP", Reg64 0L);
      ("RBP", Reg64 0L);
      ("RSI", Reg64 0L);
      ("RDI", Reg64 0L);
      ("XMM0", Reg128 (0L, 0L));
      ("XMM1", Reg128 (0L, 0L));
      ("XMM2", Reg128 (0L, 0L));
      ("XMM3", Reg128 (0L, 0L));
      ("XMM4", Reg128 (0L, 0L));
      ("XMM5", Reg128 (0L, 0L));
      ("XMM6", Reg128 (0L, 0L));
      ("XMM7", Reg128 (0L, 0L));
    ]

  let is_jmp = function
    | "JMP" | "JE" | "JNE" | "JZ" | "JG" | "JGE" | "JL" | "JLE" -> true
    | _ -> false

  (** insert elements from list to map *)
  let prep = function [] -> MapVar.empty | l -> MapVar.of_seq (List.to_seq l)

  let full_name : type a. a reg -> string t = function
    | Reg8 x -> return (Printf.sprintf "R%cX" x.[0])
    | Reg16 x -> return (Printf.sprintf "R%s" x)
    | Reg32 x -> return (Printf.sprintf "R%c%c" x.[1] x.[2])
    | Reg64 x -> return x
    | _ -> error "Isnt reg64 or less"

  let rreg = function "AH" | "BH" | "CH" | "DH" -> true | _ -> false

  let find_r : type a. var MapVar.t -> a reg -> Int64.t M.t =
   fun env reg ->
    full_name reg >>= fun name ->
    return (MapVar.find name env) >>= function
    | Reg64 x -> (
        match reg with
        | Reg8 name ->
            return
            @@
            if rreg name then rem x 0x1FFL else shift_right (rem x 0x1FFFFL) 8
        | Reg16 _ -> return @@ rem x 0x1FFFFL
        | Reg32 _ -> return @@ rem x 0x1FFFFFFFFL
        | Reg64 _ -> return x
        | _ -> error "Isnt reg64 or less")
    | _ -> error "Isnt reg64 or less"

  let find_v env name =
    return (MapVar.find name env) >>= function
    | Const x -> return x
    | _ -> error "Isnt const"

  let find_f env f =
    return (MapVar.find f env) >>= function
    | Flag x -> return x
    | _ -> error "Isnt flag"

  let change_flag env name f = return @@ MapVar.add name (Flag f) env

  let change_eflag env x y z =
    change_flag env "ZF" x >>= fun env ->
    change_flag env "SF" y >>= fun env -> change_flag env "OF" z

  let mask v l r =
    let ones l r =
      of_string
      @@ Printf.sprintf "0b0%s%s" (String.make (l - r) '1') (String.make r '0')
    in
    logand v (ones l r)

  let change_reg64 :
      type a. var MapVar.t -> Int64.t -> a reg -> var MapVar.t M.t =
   fun env v ->
    let insert ov v l r =
      let amask v l r = lognot (mask v l r) in
      let inc = add 1L in
      inc @@ add (amask ov l r) (shift_left (mask v (l - r) 0) r)
    in
    let f env reg v l r =
      full_name reg >>= fun name ->
      find_r env reg >>= fun ov ->
      return @@ insert ov v l r >>= fun v ->
      return @@ MapVar.add name (Reg64 v) env
    in
    function
    | Reg8 x -> if rreg x then f env (Reg8 x) v 8 0 else f env (Reg8 x) v 16 8
    | Reg16 x -> f env (Reg16 x) v 16 0
    | Reg32 x -> f env (Reg32 x) v 32 0
    | Reg64 x -> f env (Reg64 x) v 64 0
    | _ -> error "Isnt reg64 or less"

  let rec ev : var MapVar.t -> expr -> Int64.t M.t =
   fun env -> function
    | Add (l, r) ->
        ev env l >>= fun l ->
        ev env r >>= fun r -> return (add l r)
    | Sub (l, r) ->
        ev env l >>= fun l ->
        ev env r >>= fun r -> return (sub l r)
    | Mul (l, r) ->
        ev env l >>= fun l ->
        ev env r >>= fun r -> return (mul l r)
    | Div (l, r) ->
        ev env r >>= fun r ->
        if r = 0L then error "Division by zero"
        else ev env l >>= fun l -> return (div l r)
    | Const (ASMConst x) -> return @@ of_string x
    | Var (ASMVar x) -> find_v env x

  let inter env code s cmds =
    let tl = List.tl cmds in
    let rec assoc l = function
      | [] -> error "No such label"
      | Id label :: tl when label = l -> return tl
      | _ :: tl -> assoc l tl
    in
    let f x af =
      find_r env x >>= fun v ->
      let nv = af v in
      change_reg64 env nv x >>= fun env ->
      find_r env x >>= fun nnv ->
      change_eflag env (nv = 0L) (nv < 0L) (nv <> nnv) >>= fun env ->
      return (env, s, tl)
    in
    let c_flags env =
      find_f env "ZF" >>= fun z ->
      find_f env "SF" >>= fun s ->
      find_f env "OF" >>= fun f -> return (z, s = f)
    in
    let jmp l conde =
      c_flags env >>= fun (f, ff) ->
      if conde (f, ff) then assoc l code >>= fun tl -> return (env, s, tl)
      else return (env, s, tl)
    in
    let u_da = function
      | RegToExpr (x, y) -> ev env y >>= fun y -> return (Dyn x, y)
      | RegToReg (x, y) -> find_r env y >>= fun y -> return (Dyn x, y)
    in
    let ff x af = u_da x >>= fun (Dyn x, y) -> f x (af y) in
    match cmds with
    | Id _ :: _ -> return (env, s, tl)
    | Command x :: _ -> (
        match x with
        | RET -> return (env, s, [])
        | SYSCALL -> return (env, s, tl)
        | PUSH x -> find_r env x >>= fun v -> return (env, v :: s, tl)
        | POP x ->
            change_reg64 env (List.hd s) x >>= fun env ->
            return (env, List.tl s, tl)
        | INC x -> f x (fun x -> add (mul x 2L) 1L)
        | DEC x -> f x (fun x -> sub (mul x 2L) 1L)
        | NOT x -> f x (fun x -> lognot (mul x 2L))
        | NEG x -> f x (fun x -> neg (mul x 2L))
        | JMP x -> jmp x (fun _ -> true)
        | JE x | JZ x -> jmp x (fun (x, _) -> x)
        | JNE x -> jmp x (fun (x, _) -> not x)
        | JG x -> jmp x (fun (x, y) -> (not x) & y)
        | JGE x -> jmp x (fun (_, y) -> y)
        | JL x -> jmp x (fun (_, y) -> not y)
        | JLE x -> jmp x (fun (x, y) -> x & not y)
        | MOV x -> ff x (fun x y -> add x y)
        | ADD x -> ff x (fun x y -> add x (mul y 2L))
        | SUB x -> ff x (fun x y -> mul (sub x (mul y 2L)) minus_one)
        | IMUL x -> ff x (fun x y -> mul (add x 1L) y)
        | AND x -> ff x (fun x y -> add (logand x y) y)
        | OR x -> ff x (fun x y -> add (logor x y) y)
        | XOR x -> ff x (fun x y -> add (logxor x y) y)
        | SHL (x, y) ->
            ev env y >>= fun y -> f x (fun x -> add x (shift_left x (to_int y)))
        | SHR (x, y) ->
            ev env y >>= fun y ->
            f x (fun x -> add x (shift_right x (to_int y)))
        | CMP x ->
            u_da x >>= fun (Dyn x, y) ->
            find_r env x >>= fun x ->
            change_flag env "ZF" (x = y) >>= fun env -> return (env, s, tl))
    | _ -> return (env, s, [])

  let rec code_sec_inter env s code = function
    | cmd :: tl ->
        inter env code s (cmd :: tl) >>= fun (env, s, c) ->
        code_sec_inter env s code c
    | [] -> return (env, s, [])

  let rec data_sec_inter env s vars =
    let explode s = List.init (String.length s) (String.get s) in
    let f list len =
      let rec helper = function
        | -1 -> []
        | x when x >= List.length list -> helper (x - 1)
        | x -> List.nth list x :: helper (x - 1)
      in
      helper len
    in
    let cut b = function
      | Num x ->
          let num = of_string x in
          mask num b 0
      | Str s ->
          mask
            (List.fold_left
               (fun x y -> add (shift_left x 8) (of_int (Char.code y)))
               0L
               (f (explode s) b))
            b 0
    in
    match vars with
    | Variable (name, t, v) :: tl ->
        let vv =
          match t with
          | DB -> cut 8 v
          | DW -> cut 16 v
          | DD -> cut 32 v
          | DQ -> cut 64 v
          | DT -> cut 80 v
        in
        let vvv = Const vv in
        let env = MapVar.add name vvv env in
        data_sec_inter env s tl
    | [] -> return (env, s, [])

  let rec interpret env s = function
    | h :: tl -> (
        match h with
        | Code code ->
            code_sec_inter env s code code >>= fun (env, s, _) ->
            interpret env s tl
        | Data data ->
            data_sec_inter env s data >>= fun (env, s, _) -> interpret env s tl)
    | [] -> return (env, s, [])
end
