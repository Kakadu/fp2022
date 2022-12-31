(** Copyright 2021-2022, andreyizrailev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Utils
open Ast
open OperandsHandler

module Interpreter = struct
  type state_t =
    { reg_map : int IntMap.t
    ; xmm_reg_map : int list IntMap.t
    ; stack : int ListStack.t
    ; flags : int
    ; label_map : instruction list StringMap.t
    ; cstack : instruction list ListStack.t
    }
  [@@deriving show]

  (* Generate a map from label commands to suffixes of the instruction list.
     When jumping to label, we will obtain the instructions that we should
     execute from this map. *)
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
         { state with
           stack = ListStack.pop state.stack
         ; reg_map = reg_val_set r v state.reg_map
         })
    | _ -> failwith "Pop command operand must be a register"
  ;;

  let eval_cmp reg_map = function
    | RegReg (r1, r2) -> reg_val_get r1 reg_map - reg_val_get r2 reg_map
    | RegConst (r, c) -> reg_val_get r reg_map - const_val c
  ;;

  let eval_movdqa state = function
    | Reg r ->
      xmm_reg_val_set
        r
        [ reg_val_get (reg_name_to_dword_reg "eax") state.reg_map
        ; reg_val_get (reg_name_to_dword_reg "ebx") state.reg_map
        ; reg_val_get (reg_name_to_dword_reg "ecx") state.reg_map
        ; reg_val_get (reg_name_to_dword_reg "edx") state.reg_map
        ]
        state.xmm_reg_map
    | _ -> failwith "Movdqa command operand must be a register"
  ;;

  let eval_addpd state = function
    | RegReg (r1, r2) ->
      let r1_val = xmm_reg_val_get r1 state.xmm_reg_map in
      let r2_val = xmm_reg_val_get r2 state.xmm_reg_map in
      xmm_reg_val_set r1 (List.map2 ( + ) r1_val r2_val) state.xmm_reg_map
    | _ -> failwith "Addpd command operands must both be a registers"
  ;;

  let eval_mulpd state = function
    | RegReg (r1, r2) ->
      let r1_val = xmm_reg_val_get r1 state.xmm_reg_map in
      let r2_val = xmm_reg_val_get r2 state.xmm_reg_map in
      xmm_reg_val_set r1 (List.map2 ( * ) r1_val r2_val) state.xmm_reg_map
    | _ -> failwith "Mulpd command operands must both be a registers"
  ;;

  (* Eval B-, W-, D- or Xommand *)
  let eval_bwdxcommand state =
    let eval_helper eval_op x = { state with reg_map = eval_op state.reg_map x } in
    let eval_xmm_helper eval_op x = { state with xmm_reg_map = eval_op state x } in
    function
    | Mov x -> eval_helper eval_mov x
    | Add x -> eval_helper eval_add x
    | Sub x -> eval_helper eval_sub x
    | Inc x -> eval_helper eval_inc x
    | Mul x -> eval_helper eval_mul x
    | Push x -> eval_push state x
    | Pop x -> eval_pop state x
    | Cmp x -> { state with flags = eval_cmp state.reg_map x }
    | Movdqa x -> eval_xmm_helper eval_movdqa x
    | Addpd x -> eval_xmm_helper eval_addpd x
    | Mulpd x -> eval_xmm_helper eval_mulpd x
    | _ -> state
  ;;

  let from_label label_map l =
    match StringMap.find_opt l label_map with
    | None -> failwith (Printf.sprintf "Label %S not found in the program" l)
    | Some v -> v
  ;;

  (* Returns tuple of state and list of instructions which we should execute *)
  let eval_scommand state tl = function
    | Jmp (Label l) -> state, from_label state.label_map l
    | Je (Label l) -> state, if state.flags = 0 then from_label state.label_map l else tl
    | Jne (Label l) ->
      state, if state.flags <> 0 then from_label state.label_map l else tl
    | Call (Label l) ->
      { state with cstack = ListStack.push tl state.cstack }, from_label state.label_map l
    | _ -> failwith "Command not supported"
  ;;

  let eval_ret state =
    match ListStack.peek state.cstack with
    | None -> failwith "Cannot return from function, the call stack is empty"
    | Some instrs -> { state with cstack = ListStack.pop state.cstack }, instrs
  ;;

  let rec eval state = function
    | [] -> state
    | instr :: tl ->
      (match instr with
       (* We do not care, labels are taken into account in the label_map *)
       | LCommand _ -> eval state tl
       | BCommand Ret ->
         (match eval_ret state with
          | state, instrs -> eval state instrs)
       | BCommand x -> eval (eval_bwdxcommand state x) tl
       | WCommand x -> eval (eval_bwdxcommand state x) tl
       | DCommand x -> eval (eval_bwdxcommand state x) tl
       | XCommand x -> eval (eval_bwdxcommand state x) tl
       | SCommand x ->
         (match eval_scommand state tl x with
          | state, instrs -> eval state instrs))
  ;;

  let eval_whole whole_program =
    let initial_label_map = gen_label_map whole_program in
    (* Each of dword registers is associated with 0 initial value *)
    let gen_initial_reg_map init reg_name_list =
      List.fold_left
        (fun map reg_name -> IntMap.add (reg_name_to_id reg_name) init map)
        IntMap.empty
        reg_name_list
    in
    let initial_reg_map = gen_initial_reg_map 0 dword_reg_name_list in
    let initial_xmm_reg_map = gen_initial_reg_map [ 0; 0; 0; 0 ] xmm_reg_name_list in
    let initial_state =
      { reg_map = initial_reg_map
      ; xmm_reg_map = initial_xmm_reg_map
      ; stack = ListStack.empty
      ; flags = 0
      ; label_map = initial_label_map
      ; cstack = ListStack.empty
      }
    in
    validate_ast whole_program;
    eval initial_state whole_program
  ;;
end

open Interpreter

let%test _ =
  let program =
    [ WCommand (Mov (RegConst (reg_name_to_word_reg "ax", int_to_word_const 3)))
    ; WCommand (Add (RegConst (reg_name_to_word_reg "ax", int_to_word_const 2)))
    ]
  in
  let final_reg_map = (eval_whole program).reg_map in
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
  let final_reg_map = (eval_whole program).reg_map in
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
  let final_reg_map = (eval_whole program).reg_map in
  reg_val_get (reg_name_to_word_reg "ax") final_reg_map = ((7 * 256) + 6 + 1) * 3
  && reg_val_get (reg_name_to_dword_reg "edx") final_reg_map = 43
  && reg_val_get (reg_name_to_byte_reg "bh") final_reg_map = 8
;;

let%test _ =
  let program =
    [ WCommand (Mov (RegConst (reg_name_to_word_reg "ax", int_to_word_const 4)))
    ; BCommand (Cmp (RegConst (reg_name_to_byte_reg "al", int_to_byte_const 5)))
    ]
  in
  let final_flags = (eval_whole program).flags in
  final_flags < 0
;;

let%test _ =
  let program =
    [ WCommand (Mov (RegConst (reg_name_to_word_reg "ax", int_to_word_const 1)))
    ; SCommand (Jmp (Label "l1"))
    ; WCommand (Mov (RegConst (reg_name_to_word_reg "ax", int_to_word_const 2)))
    ; LCommand "l1"
    ; WCommand (Mov (RegConst (reg_name_to_word_reg "bx", int_to_word_const 1)))
    ; WCommand (Cmp (RegReg (reg_name_to_word_reg "ax", reg_name_to_word_reg "bx")))
    ; SCommand (Je (Label "l2"))
    ; WCommand (Mov (RegConst (reg_name_to_word_reg "cx", int_to_word_const 3)))
    ; SCommand (Jmp (Label "exit"))
    ; LCommand "l2"
    ; WCommand (Mov (RegConst (reg_name_to_word_reg "cx", int_to_word_const 4)))
    ; LCommand "exit"
    ]
  in
  let final_reg_map = (eval_whole program).reg_map in
  reg_val_get (reg_name_to_word_reg "cx") final_reg_map = 4
;;

let%test _ =
  let program =
    [ WCommand (Mov (RegConst (reg_name_to_word_reg "ax", int_to_word_const 1)))
    ; SCommand (Jmp (Label "l1"))
    ; WCommand (Mov (RegConst (reg_name_to_word_reg "ax", int_to_word_const 2)))
    ; LCommand "l1"
    ; WCommand (Mov (RegConst (reg_name_to_word_reg "bx", int_to_word_const 1)))
    ; WCommand (Cmp (RegReg (reg_name_to_word_reg "ax", reg_name_to_word_reg "bx")))
    ; SCommand (Jne (Label "l2"))
    ; WCommand (Mov (RegConst (reg_name_to_word_reg "cx", int_to_word_const 3)))
    ; SCommand (Jmp (Label "exit"))
    ; LCommand "l2"
    ; WCommand (Mov (RegConst (reg_name_to_word_reg "cx", int_to_word_const 4)))
    ; LCommand "exit"
    ]
  in
  let final_reg_map = (eval_whole program).reg_map in
  reg_val_get (reg_name_to_word_reg "cx") final_reg_map = 3
;;

let%test _ =
  let program =
    [ WCommand (Mov (RegConst (reg_name_to_word_reg "ax", int_to_word_const 1)))
    ; SCommand (Jmp (Label "l1"))
    ; WCommand (Mov (RegConst (reg_name_to_word_reg "ax", int_to_word_const 2)))
    ; LCommand "l1"
    ; WCommand (Mov (RegConst (reg_name_to_word_reg "bx", int_to_word_const 43)))
    ; WCommand (Cmp (RegReg (reg_name_to_word_reg "ax", reg_name_to_word_reg "bx")))
    ; SCommand (Je (Label "l2"))
    ; WCommand (Mov (RegConst (reg_name_to_word_reg "cx", int_to_word_const 3)))
    ; SCommand (Jmp (Label "exit"))
    ; LCommand "l2"
    ; WCommand (Mov (RegConst (reg_name_to_word_reg "cx", int_to_word_const 4)))
    ; LCommand "exit"
    ]
  in
  let final_reg_map = (eval_whole program).reg_map in
  reg_val_get (reg_name_to_word_reg "cx") final_reg_map = 3
;;

let%test _ =
  let program =
    [ DCommand (Mov (RegConst (reg_name_to_dword_reg "eax", int_to_dword_const 9)))
    ; SCommand (Call (Label "fib"))
    ; SCommand (Jmp (Label "end"))
    ; LCommand "fib"
    ; DCommand (Cmp (RegConst (reg_name_to_dword_reg "eax", int_to_dword_const 0)))
    ; SCommand (Jne (Label "l1"))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "ebx", int_to_dword_const 0)))
    ; BCommand Ret
    ; LCommand "l1"
    ; DCommand (Cmp (RegConst (reg_name_to_dword_reg "eax", int_to_dword_const 1)))
    ; SCommand (Jne (Label "l2"))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "ebx", int_to_dword_const 1)))
    ; BCommand Ret
    ; LCommand "l2"
    ; DCommand (Push (Reg (reg_name_to_dword_reg "eax")))
    ; DCommand (Sub (RegConst (reg_name_to_dword_reg "eax", int_to_dword_const 2)))
    ; SCommand (Call (Label "fib"))
    ; DCommand (Pop (Reg (reg_name_to_dword_reg "eax")))
    ; DCommand (Push (Reg (reg_name_to_dword_reg "ebx")))
    ; DCommand (Sub (RegConst (reg_name_to_dword_reg "eax", int_to_dword_const 1)))
    ; SCommand (Call (Label "fib"))
    ; DCommand (Pop (Reg (reg_name_to_dword_reg "ecx")))
    ; DCommand (Add (RegReg (reg_name_to_dword_reg "ebx", reg_name_to_dword_reg "ecx")))
    ; BCommand Ret
    ; LCommand "end"
    ]
  in
  let final_reg_map = (eval_whole program).reg_map in
  reg_val_get (reg_name_to_dword_reg "ebx") final_reg_map = 34
;;

let%test _ =
  let program =
    [ DCommand (Mov (RegConst (reg_name_to_dword_reg "eax", int_to_dword_const 1)))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "ebx", int_to_dword_const 2)))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "ecx", int_to_dword_const 3)))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "edx", int_to_dword_const 4)))
    ; XCommand (Movdqa (Reg (reg_name_to_xmm_reg "xmm0")))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "eax", int_to_dword_const 5)))
    ; XCommand (Movdqa (Reg (reg_name_to_xmm_reg "xmm7")))
    ; XCommand (Addpd (RegReg (reg_name_to_xmm_reg "xmm0", reg_name_to_xmm_reg "xmm7")))
    ]
  in
  let final_xmm_reg_map = (eval_whole program).xmm_reg_map in
  List.equal
    ( = )
    (xmm_reg_val_get (reg_name_to_xmm_reg "xmm0") final_xmm_reg_map)
    [ 6; 4; 6; 8 ]
;;

(* Calculate (1, 2, 3) x ((4, 5, 6), (7, 8, 9), (10, 11, 12)).
   The answer is located in 3 first dwords of xmm0 *)
let%test _ =
  let program =
    [ DCommand (Mov (RegConst (reg_name_to_dword_reg "eax", int_to_dword_const 1)))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "ebx", int_to_dword_const 1)))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "ecx", int_to_dword_const 1)))
    ; XCommand (Movdqa (Reg (reg_name_to_xmm_reg "xmm0")))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "eax", int_to_dword_const 2)))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "ebx", int_to_dword_const 2)))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "ecx", int_to_dword_const 2)))
    ; XCommand (Movdqa (Reg (reg_name_to_xmm_reg "xmm1")))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "eax", int_to_dword_const 3)))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "ebx", int_to_dword_const 3)))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "ecx", int_to_dword_const 3)))
    ; XCommand (Movdqa (Reg (reg_name_to_xmm_reg "xmm2")))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "eax", int_to_dword_const 4)))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "ebx", int_to_dword_const 5)))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "ecx", int_to_dword_const 6)))
    ; XCommand (Movdqa (Reg (reg_name_to_xmm_reg "xmm3")))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "eax", int_to_dword_const 7)))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "ebx", int_to_dword_const 8)))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "ecx", int_to_dword_const 9)))
    ; XCommand (Movdqa (Reg (reg_name_to_xmm_reg "xmm4")))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "eax", int_to_dword_const 10)))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "ebx", int_to_dword_const 11)))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "ecx", int_to_dword_const 12)))
    ; XCommand (Movdqa (Reg (reg_name_to_xmm_reg "xmm5")))
    ; XCommand (Mulpd (RegReg (reg_name_to_xmm_reg "xmm0", reg_name_to_xmm_reg "xmm3")))
    ; XCommand (Mulpd (RegReg (reg_name_to_xmm_reg "xmm1", reg_name_to_xmm_reg "xmm4")))
    ; XCommand (Mulpd (RegReg (reg_name_to_xmm_reg "xmm2", reg_name_to_xmm_reg "xmm5")))
    ; XCommand (Addpd (RegReg (reg_name_to_xmm_reg "xmm0", reg_name_to_xmm_reg "xmm1")))
    ; XCommand (Addpd (RegReg (reg_name_to_xmm_reg "xmm0", reg_name_to_xmm_reg "xmm2")))
    ]
  in
  let final_xmm_reg_map = (eval_whole program).xmm_reg_map in
  List.equal
    ( = )
    (xmm_reg_val_get (reg_name_to_xmm_reg "xmm0") final_xmm_reg_map)
    [ 48; 54; 60; 0 ]
;;

let%test _ =
  let program = [ WCommand (Inc (Const (int_to_word_const 7))) ] in
  try
    ignore (eval_whole program);
    false
  with
  | Failure _ -> true
;;
