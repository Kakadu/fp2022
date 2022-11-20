(** Copyright 2021-2022, andreyizrailev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Utils
open Ast
open Ast.OperandsHandler

module Interpreter = struct
  (* Current state of execution *)
  type state_t =
    { (* Map from register ids to their values.
         Note that we won't have registers like "ax" here since their value can be
         obtained from "eax". So, the map only contains "primary" registers, i.e.
         ones that are not part of the others *)
      reg_map : int IntMap.t
    ; stack : int ListStack.t (* Program stack in an oridnary sense *)
    }
  [@@deriving show]

  (* Each of dword registers is associated with 0 initial value *)
  let initial_reg_map =
    List.fold_left
      (fun map reg_name -> IntMap.add (reg_name_to_id reg_name) 0 map)
      IntMap.empty
      dword_reg_name_list
  ;;

  let initial_state = { reg_map = initial_reg_map; stack = ListStack.empty }

  (* Generate a map from label commands to suffixes of the instruction list.
     When jumping to label, we will obtain the instructions that we should
     executed from this map. *)
  let gen_label_map whole_program =
    let rec helper m = function
      | [] -> m
      | LCommand s :: tl -> helper (StringMap.add s tl m) tl
      | _ :: tl -> helper m tl
    in
    helper StringMap.empty whole_program
  ;;

  let eval_mov reg_map = function
    | RegReg (r1, r2) -> reg_val_set r1 (reg_val_get r2 reg_map) reg_map
    | RegConst (r, c) -> reg_val_set r (const_val c) reg_map
  ;;

  let eval_add reg_map = function
    | RegReg (r1, r2) ->
      let r1_val = reg_val_get r1 reg_map in
      let r2_val = reg_val_get r2 reg_map in
      reg_val_set r1 (r1_val + r2_val) reg_map
    | RegConst (r, c) ->
      let r_val = reg_val_get r reg_map in
      let c_val = const_val c in
      reg_val_set r (r_val + c_val) reg_map
  ;;

  let eval_sub reg_map = function
    | RegReg (r1, r2) ->
      let r1_val = reg_val_get r1 reg_map in
      let r2_val = reg_val_get r2 reg_map in
      reg_val_set r1 (r1_val - r2_val) reg_map
    | RegConst (r, c) ->
      let r_val = reg_val_get r reg_map in
      let c_val = const_val c in
      reg_val_set r (r_val - c_val) reg_map
  ;;

  let eval_inc reg_map = function
    | Reg r -> reg_val_set r (reg_val_get r reg_map + 1) reg_map
    | _ -> failwith "Inc command operand must be a register"
  ;;

  let eval_mul reg_map x =
    let eax_val = reg_val_get (reg_name_to_dword_reg "eax") reg_map in
    match x with
    | Reg r ->
      let r_val = reg_val_get r reg_map in
      reg_val_set (reg_name_to_dword_reg "eax") (eax_val * r_val) reg_map
    | Const c ->
      let c_val = const_val c in
      reg_val_set (reg_name_to_dword_reg "eax") (eax_val * c_val) reg_map
    | _ -> failwith "Mul command operand must be a register or a constant"
  ;;

  let eval_push state = function
    (* We assume that we may push any register's value, i.e. "push ah"
       is a valid command *)
    | Reg r ->
      { state with stack = ListStack.push (reg_val_get r state.reg_map) state.stack }
    | Const c -> { state with stack = ListStack.push (const_val c) state.stack }
    | _ -> failwith "Push command operand must be a register or a constant"
  ;;

  let eval_pop state = function
    | Reg r ->
      let value = ListStack.peek state.stack in
      (match value with
       | None -> failwith "Trying to pop when the stack is empty"
       | Some v ->
         { stack = ListStack.pop state.stack; reg_map = reg_val_set r v state.reg_map })
    | _ -> failwith "Pop command operand must be a register"
  ;;

  (* Eval B-, W- or DCommand *)
  let eval_bwdcommand state = function
    | Mov x -> { state with reg_map = eval_mov state.reg_map x }
    | Add x -> { state with reg_map = eval_add state.reg_map x }
    | Sub x -> { state with reg_map = eval_sub state.reg_map x }
    | Inc x -> { state with reg_map = eval_inc state.reg_map x }
    | Mul x -> { state with reg_map = eval_mul state.reg_map x }
    | Push x -> eval_push state x
    | Pop x -> eval_pop state x
    | _ -> state
  ;;

  let rec eval state = function
    | [] -> state
    | instr :: tl ->
      (match instr with
       (* We do not care, labels are taken into account in the label_map *)
       | LCommand _ -> eval state tl
       | BCommand x -> eval (eval_bwdcommand state x) tl
       | WCommand x -> eval (eval_bwdcommand state x) tl
       | DCommand x -> eval (eval_bwdcommand state x) tl
       | _ -> state)
  ;;
end

open Interpreter

let%test _ =
  let program =
    [ WCommand (Mov (RegConst (reg_name_to_word_reg "ax", int_to_word_const 3)))
    ; WCommand (Add (RegConst (reg_name_to_word_reg "ax", int_to_word_const 2)))
    ]
  in
  let final_reg_map = (eval initial_state program).reg_map in
  reg_val_get (reg_name_to_byte_reg "al") final_reg_map = 5
;;

let%test _ =
  let program =
    [ LCommand "label"
    ; WCommand (Mov (RegConst (reg_name_to_word_reg "ax", int_to_word_const 8)))
    ; WCommand (Sub (RegConst (reg_name_to_word_reg "ax", int_to_word_const 2)))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "ebx", int_to_dword_const 7)))
    ; BCommand (Add (RegReg (reg_name_to_byte_reg "ah", reg_name_to_byte_reg "bl")))
    ; WCommand (Inc (Reg (reg_name_to_word_reg "ax")))
    ; BCommand (Mov (RegConst (reg_name_to_byte_reg "cl", int_to_byte_const 3)))
    ; DCommand (Mul (Reg (reg_name_to_dword_reg "ecx")))
    ]
  in
  let final_reg_map = (eval initial_state program).reg_map in
  reg_val_get (reg_name_to_word_reg "ax") final_reg_map = ((7 * 256) + 6 + 1) * 3
;;

let%test _ =
  let program =
    [ LCommand "label"
    ; WCommand (Mov (RegConst (reg_name_to_word_reg "ax", int_to_word_const 8)))
    ; DCommand (Push (Reg (reg_name_to_dword_reg "eax")))
    ; WCommand (Sub (RegConst (reg_name_to_word_reg "ax", int_to_word_const 2)))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "ebx", int_to_dword_const 7)))
    ; BCommand (Push (Const (int_to_byte_const 43)))
    ; BCommand (Add (RegReg (reg_name_to_byte_reg "ah", reg_name_to_byte_reg "bl")))
    ; WCommand (Inc (Reg (reg_name_to_word_reg "ax")))
    ; BCommand (Mov (RegConst (reg_name_to_byte_reg "cl", int_to_byte_const 3)))
    ; WCommand (Pop (Reg (reg_name_to_word_reg "dx")))
    ; DCommand (Mul (Reg (reg_name_to_dword_reg "ecx")))
    ; BCommand (Pop (Reg (reg_name_to_byte_reg "bh")))
    ]
  in
  let final_reg_map = (eval initial_state program).reg_map in
  reg_val_get (reg_name_to_word_reg "ax") final_reg_map = ((7 * 256) + 6 + 1) * 3
  && reg_val_get (reg_name_to_dword_reg "edx") final_reg_map = 43
  && reg_val_get (reg_name_to_byte_reg "bh") final_reg_map = 8
;;
