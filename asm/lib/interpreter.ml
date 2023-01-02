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
    | Const of string  (** global consts *)
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
    | Reg8 x -> return (Printf.sprintf "R%cX" x.[1])
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
            if rreg name then logand x 0xFFL
            else shift_left (logand x 0xFFFFL) 8
        | Reg16 _ -> return @@ logand x 0xFFFFL
        | Reg32 _ -> return @@ logand x 0xFFFFFFFFL
        | Reg64 _ -> return x
        | _ -> error "Isnt reg64 or less")
    | _ -> error "Isnt reg64 or less"

  let find_v env name =
    return (MapVar.find name env) >>= function
    | Const x -> return @@ of_string x
    | _ -> error "Isnt const"

  let find_f env f =
    return (MapVar.find f env) >>= function
    | Flag x -> return x
    | _ -> error "Isnt flag"

  let change_flag env name f = return @@ MapVar.add name (Flag f) env

  let change_eflag env x y z =
    change_flag env "ZF" x >>= fun env ->
    change_flag env "SF" y >>= fun env -> change_flag env "OF" z

  let change_reg64 :
      type a. var MapVar.t -> Int64.t -> a reg -> var MapVar.t M.t =
   fun env v ->
    let mask v l r =
      add (logand v (sub (shift_left 1L l) 1L)) (div v (shift_left 1L r))
    in
    let insert ov v l r =
      add
        (sub ov (mask ov l r))
        (logand (shift_left v r) (sub (shift_left v l) 1L))
    in
    let f env reg v l r =
      full_name reg >>= fun name ->
      find_r env (Reg64 name) >>= fun ov ->
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

  let inter env code (h :: stl) (cmd :: tl) =
    let s = h :: stl in
    let f x af =
      find_r env x >>= fun ov ->
      return (af ov) >>= fun nv ->
      change_reg64 env nv x >>= fun env ->
      find_r env x >>= fun nnv ->
      change_eflag env (nv = 0L) (nv < 0L) (nv != nnv) >>= fun env ->
      return (env, s, tl)
    in
    let rec assoc l = function
      | [] -> error ""
      | Id label :: tl when label = l -> return tl
      | _ :: tl -> assoc l tl
    in
    let rec c_flags env =
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
      | RegToReg (x, y) -> find_r env y >>= fun y -> return (Dyn x, y)
      | RegToExpr (x, y) -> ev env y >>= fun y -> return (Dyn x, y)
    in
    let ff x af = u_da x >>= fun (Dyn x, y) -> f x (af y) in
    match cmd with
    | Id _ -> return (env, s, tl)
    | Command x -> (
        match x with
        | RET -> return (env, s, [])
        | SYSCALL -> return (env, s, tl) (*TODO*)
        | PUSH x -> return (env, Dyn x :: s, tl)
        | POP x ->
            find_r env x >>= fun v ->
            change_reg64 env v x >>= fun env -> return (env, stl, tl)
        | INC x -> f x (add 1L)
        | DEC x -> f x (sub 1L)
        | NOT x -> f x lognot
        | NEG x -> f x neg
        | JMP x -> jmp x (fun _ -> true)
        | JE x | JZ x -> jmp x (fun (x, _) -> x)
        | JNE x -> jmp x (fun (x, _) -> not x)
        | JG x -> jmp x (fun (x, y) -> (not x) & y)
        | JGE x -> jmp x (fun (_, y) -> y)
        | JL x -> jmp x (fun (_, y) -> not y)
        | JLE x -> jmp x (fun (x, y) -> x & not y)
        | MOV x -> ff x (fun x _ -> x)
        | ADD x -> ff x add
        | SUB x -> ff x sub
        | IMUL x -> ff x mul
        | AND x -> ff x logand
        | XOR x -> ff x logxor
        | OR x -> ff x logor
        | SHL (x, y) ->
            ev env y >>= fun y -> f x (fun x -> shift_left x (to_int y))
        | SHR (x, y) ->
            ev env y >>= fun y -> f x (fun x -> shift_right x (to_int y))
        | CMP x ->
            u_da x >>= fun (Dyn x, y) ->
            find_r env x >>= fun x ->
            change_flag env "ZF" (x = y) >>= fun env -> return (env, s, tl))

  let rec code_sec_inter env s code = function
    | cmd :: tl ->
        inter env code s (cmd :: tl) >>= fun (env, s, c) ->
        code_sec_inter env s code c
    | [] -> return (env, s, [])

  (*
     (** not implemeted data secction interpreter *)
     let data_sec_inter env st = function _ -> return (env, st)

     (** code section interpreter that return map, ast - general code that shouldn't change *)
     let rec code_sec_inter env st ast =
       let jump env ast tl = function
         | Args1 (Mnemonic cmd, Lab label) ->
             assoc label ast >>= fun code ->
             (match cmd with
             | "JMP" -> return true
             | "JE" | "JZ" -> find_flag_cont env "ZF"
             | "JNE" -> find_flag_cont env "ZF" >>= fun z -> return @@ not z
             | "JG" -> find_eflags env >>= fun (z, s, o) -> return (o = s && not z)
             | "JGE" -> find_eflags env >>= fun (_, s, o) -> return (o = s)
             | "JL" -> find_eflags env >>= fun (_, s, o) -> return (o != s)
             | "JLE" -> find_eflags env >>= fun (z, s, o) -> return (z && o != s)
             | x -> error @@ x ^ " is not a jmp")
             >>= fun cond ->
             if cond then code_sec_inter env st ast code
             else code_sec_inter env st ast tl
         | _ -> error "Isnt jmp"
       in
       function
       | Command cmd :: tl -> (
           match cmd with
           | Args1 (Mnemonic c, _) when is_jmp c -> jump env ast tl cmd
           | _ ->
               inter_cmd env st cmd >>= fun (env, st) ->
               code_sec_inter env st ast tl)
       | Id _ :: tl -> code_sec_inter env st ast tl
       | [] -> return (env, st)

     (** general general interpreter *)
     let rec interpret env st = function
       | [] -> return (env, st)
       | h :: tl -> (
           match h with
           | Code code ->
               t_ch code >>= fun _ ->
               code_sec_inter env st code code >>= fun (env, st) ->
               interpret env st tl
           | _ -> error "Not implemented yet")
     (* data_sec_inter st env data >>= fun (env, st) -> interpret env st tl) *)
  *)
end
