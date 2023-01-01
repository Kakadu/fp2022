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

  (*
     (** interpret command and return map and stack *)
     let inter_one_args_cmd env st arg1 =
       let helper fu = change_reg64 env fu arg1 >>= fun env -> return (env, st) in
       function
       | "INC" -> helper (fun x -> add x 1L)
       | "DEC" -> helper (fun x -> sub x 1L)
       | "NOT" -> helper neg
       | "NEG" -> helper (fun x -> add (neg x) 1L)
       | "PUSH" ->
           find_reg64_cont env arg1 >>= fun v -> return (env, (arg1, Reg64 v) :: st)
       | "POP" ->
           return
             (MapVar.add arg1 (List.assoc arg1 st) env, List.remove_assoc arg1 st)
       (* | "CALL"  *)
       | x -> error (x ^ " is not implemented yet")

     (** interpret command and return map *)
     let inter_zero_args_cmd env = function
       | "RET" -> return env
       | "SYSCALL" | _ -> error "Not implemented yet"

     (** interpret command and return map *)
     let inter_two_args_cmd env arg1 arg2 cmd =
       let f = function
         | Ast.Const c -> return @@ of_string c
         | Ast.Reg64 reg_name -> find_reg64_cont env reg_name
         | Ast.Reg32 _ | Ast.Reg16 _ | Ast.Reg8 _ | _ ->
             error "Vars not implemented"
       in
       let helper fu = change_reg64 env fu arg1 in
       ev f arg2 >>= fun arg2 ->
       match cmd with
       | "MOV" -> helper (fun _ -> arg2)
       | "ADD" -> helper (fun x -> add x arg2)
       | "SUB" -> helper (fun x -> sub x arg2)
       | "IMUL" -> helper (fun x -> mul x arg2)
       | "CMP" ->
           find_reg64_cont env arg1 >>= fun arg1 ->
           change_flag env (arg1 = arg2) "ZF" >>= fun env ->
           change_flag env (arg1 < arg2) "SF"
       | "AND" -> helper (fun x -> logand x arg2)
       | "XOR" -> helper (fun x -> logxor x arg2)
       | "OR" -> helper (fun x -> logor x arg2)
       | "SHL" -> helper (fun x -> shift_left x (to_int arg2))
       | "SHR" -> helper (fun x -> shift_right x (to_int arg2))
       (* | "ADDPD" | "SUBPD" | "MULPD" | "MOVAPD" *)
       | _ -> error "Is not a mnemonic"

     (** general interpreter of commands not including jmp commands *)
     let inter_cmd env st = function
       | Args0 (Mnemonic cmd) ->
           inter_zero_args_cmd env cmd >>= fun env -> return (env, st)
       | Args1 (Mnemonic cmd, Ast.Reg64 x) -> inter_one_args_cmd env st x cmd
       | Args2 (Mnemonic cmd, Ast.Reg64 x, y) ->
           inter_two_args_cmd env x y cmd >>= fun env -> return (env, st)
       | _ -> error "Isnt argsn"

     (** returns list of code_section that placed after label l *)
     let rec assoc (Label l) = function
       | [] -> error ("No such label: " ^ l)
       | Id (Label label) :: tl when label = l -> return tl
       | _ :: tl -> assoc (Label l) tl

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
