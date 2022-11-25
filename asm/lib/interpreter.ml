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
    | Reg64 of int64  (** not so large registers *)
    | Reg128 of string  (** large registers *)
    | Const of string  (** global consts *)
  [@@deriving show { with_path = false }]

  type envr = var MapVar.t [@@deriving show { with_path = false }]

  (** start values of registers *)
  let r_list =
    [
      (* "EFLAGS", Reg64 0 *)
      ("CMPFLAG", Reg64 0L);
      ("RAX", Reg64 0L);
      ("RBX", Reg64 0L);
      ("RCX", Reg64 0L);
      ("RDX", Reg64 0L);
      ("RSP", Reg64 0L);
      ("RBP", Reg64 0L);
      ("RSI", Reg64 0L);
      ("RDI", Reg64 0L);
      ("XMM0", Reg128 "0");
      ("XMM1", Reg128 "0");
      ("XMM2", Reg128 "0");
      ("XMM3", Reg128 "0");
      ("XMM4", Reg128 "0");
      ("XMM5", Reg128 "0");
      ("XMM6", Reg128 "0");
      ("XMM7", Reg128 "0");
    ]

  let is_jmp = function
    | "JMP" | "JE" | "JNE" | "JZ" | "JG" | "JGE" | "JL" | "JLE" -> true
    | _ -> false

  (** insert elements from list to map *)
  let prep = function [] -> MapVar.empty | l -> MapVar.of_seq (List.to_seq l)

  (** calculate a expression, f function that takes values of registers or constans from map or calcuclate integer constants from expression *)
  let rec ev f = function
    | Add (l, r) ->
        ev f l >>= fun l ->
        ev f r >>= fun r -> return (add l r)
    | Sub (l, r) ->
        ev f l >>= fun l ->
        ev f r >>= fun r -> return (sub l r)
    | Mul (l, r) ->
        ev f l >>= fun l ->
        ev f r >>= fun r -> return (mul l r)
    | Div (l, r) ->
        ev f r >>= fun r ->
        if r = 0L then error "Division by zero"
        else ev f l >>= fun l -> return (div l r)
    | const -> f const

  (** return value of register named 'name' *)
  let find_reg64_cont env name =
    return (MapVar.find name env) >>= function
    | Reg64 x -> return x
    | _ -> error "not a R64"

  (** change value of register by function f *)
  let change_reg64 env f name =
    find_reg64_cont env name >>= fun reg ->
    return @@ MapVar.add name (Reg64 (f reg)) env

  (** interpret command and return map *)
  let inter_one_args_cmd env arg1 =
    let helper fu = change_reg64 env fu arg1 in
    function
    | "INC" -> helper (fun x -> add x 1L)
    | "DEC" -> helper (fun x -> sub x 1L)
    | "NOT" -> helper neg
    | "NEG" -> helper (fun x -> add (neg x) 1L)
    (* | "PUSH" | "POP" | "CALL"  *)
    | x -> error (x ^ " is not implemented yet")

  (** interpret command and return map *)
  let inter_zero_args_cmd env = function
    | "RET" -> return env
    | "SYSCALL" | _ -> error "Not implemented yet"

  (** interpret command and return map *)
  let inter_two_args_cmd env arg1 arg2 cmd =
    let f = function
      | Ast.Const c -> return @@ of_string c
      | Ast.Reg reg_name -> find_reg64_cont env reg_name
      | _ -> error "Vars not implemented"
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
        change_reg64 env (fun _ -> if arg1 = arg2 then 1L else 0L) "CMPFLAG"
    | "AND" -> helper (fun x -> logand x arg2)
    | "XOR" -> helper (fun x -> logxor x arg2)
    | "OR" -> helper (fun x -> logor x arg2)
    | "SHL" | "SHR" | _ -> error "Not implemented yet"

  (** general interpreter of commands not including jmp commands *)
  let inter_cmd env = function
    | Args0 (Mnemonic cmd) -> inter_zero_args_cmd env cmd
    | Args1 (Mnemonic cmd, Ast.Reg x) -> inter_one_args_cmd env x cmd
    | Args2 (Mnemonic cmd, Ast.Reg x, y) -> inter_two_args_cmd env x y cmd
    | _ -> error "Isnt argsn"

  (** returns list of code_section that placed after label l *)
  let rec assoc (Label l) = function
    | [] -> error ("No such label: " ^ l)
    | Id (Label label) :: tl when label = l -> return tl
    | _ :: tl -> assoc (Label l) tl

  (** not implemeted data secction interpreter *)
  let data_sec_inter env = function _ -> return env

  (** code section interpreter that return map, ast - general code that shouldn't change *)
  let rec code_sec_inter env ast =
    let jump env ast tl = function
      | Args1 (Mnemonic cmd, Lab label) -> (
          assoc label ast >>= fun code ->
          match cmd with
          | "JMP" -> code_sec_inter env ast code
          | "JE" ->
              find_reg64_cont env "CMPFLAG" >>= fun flag ->
              if flag == 1L then code_sec_inter env ast code
              else code_sec_inter env ast tl
          | "JNE" | "JZ" | "JG" | "JGE" | "JL" | "JLE" | _ -> error "")
      | _ -> error "Isnt jmp"
    in
    function
    | Command cmd :: tl -> (
        match cmd with
        | Args1 (Mnemonic c, _) when is_jmp c -> jump env ast tl cmd
        | Args0 _ | Args1 _ | Args2 _ ->
            inter_cmd env cmd >>= fun env -> code_sec_inter env ast tl)
    | Id _ :: tl -> code_sec_inter env ast tl
    | [] -> return env

  (** general general interpreter *)
  let rec interpret env = function
    | [] -> return env
    | h :: tl -> (
        match h with
        | Code code ->
            code_sec_inter env code code >>= fun env -> interpret env tl
        | Data data -> data_sec_inter env data >>= fun env -> interpret env tl)
end