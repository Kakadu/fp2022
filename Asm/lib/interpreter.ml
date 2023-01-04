(** Copyright 2021-2023, andreyizrailev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Utils
open MonadError

module Interpreter (M : MonadError) = struct
  open Ast.Ast (M)
  open OperandsHandler.OperandsHandler (M)
  open M

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
    | RegReg (r1, r2) -> reg_val_get r2 reg_map >>= fun v -> reg_val_set r1 v reg_map
    | RegConst (r, c) -> reg_val_set r (const_val c) reg_map
  ;;

  let eval_add reg_map = function
    | RegReg (r1, r2) ->
      let* r1_val = reg_val_get r1 reg_map in
      let* r2_val = reg_val_get r2 reg_map in
      reg_val_set r1 (r1_val + r2_val) reg_map
    | RegConst (r, c) ->
      let* r_val = reg_val_get r reg_map in
      let c_val = const_val c in
      reg_val_set r (r_val + c_val) reg_map
  ;;

  let eval_sub reg_map = function
    | RegReg (r1, r2) ->
      let* r1_val = reg_val_get r1 reg_map in
      let* r2_val = reg_val_get r2 reg_map in
      reg_val_set r1 (r1_val - r2_val) reg_map
    | RegConst (r, c) ->
      let* r_val = reg_val_get r reg_map in
      let c_val = const_val c in
      reg_val_set r (r_val - c_val) reg_map
  ;;

  let eval_inc reg_map = function
    | Reg r -> reg_val_get r reg_map >>= fun v -> reg_val_set r (v + 1) reg_map
    | _ -> error "Inc command operand must be a register"
  ;;

  let eval_mul reg_map x =
    let* eax = reg_name_to_dword_reg "eax" in
    let* eax_val = reg_val_get eax reg_map in
    match x with
    | Reg r ->
      let* r_val = reg_val_get r reg_map in
      reg_val_set eax (eax_val * r_val) reg_map
    | Const c ->
      let c_val = const_val c in
      reg_val_set eax (eax_val * c_val) reg_map
    | _ -> error "Mul command operand must be a register or a constant"
  ;;

  let eval_push state = function
    (* We assume that we may push any register's value, i.e. "push ah"
       is a valid command *)
    | Reg r ->
      let* v = reg_val_get r state.reg_map in
      return { state with stack = ListStack.push v state.stack }
    | Const c -> return { state with stack = ListStack.push (const_val c) state.stack }
    | _ -> error "Push command operand must be a register or a constant"
  ;;

  let eval_pop state = function
    | Reg r ->
      let value = ListStack.peek state.stack in
      (match value with
       | None -> error "Trying to pop when the stack is empty"
       | Some v ->
         (match ListStack.pop state.stack with
          | None ->
            error
              "Critical error: ListStack.pop failed while the previous ListStack.peek \
               succeded"
          | Some s ->
            let* reg_map_new = reg_val_set r v state.reg_map in
            return { state with stack = s; reg_map = reg_map_new }))
    | _ -> error "Pop command operand must be a register"
  ;;

  let eval_cmp reg_map = function
    | RegReg (r1, r2) ->
      let* r1_val = reg_val_get r1 reg_map in
      let* r2_val = reg_val_get r2 reg_map in
      return (r1_val - r2_val)
    | RegConst (r, c) ->
      let* r_val = reg_val_get r reg_map in
      return (r_val - const_val c)
  ;;

  let eval_movdqa state = function
    | Reg r ->
      let* eax_val =
        reg_name_to_dword_reg "eax" >>= fun r -> reg_val_get r state.reg_map
      in
      let* ebx_val =
        reg_name_to_dword_reg "ebx" >>= fun r -> reg_val_get r state.reg_map
      in
      let* ecx_val =
        reg_name_to_dword_reg "ecx" >>= fun r -> reg_val_get r state.reg_map
      in
      let* edx_val =
        reg_name_to_dword_reg "edx" >>= fun r -> reg_val_get r state.reg_map
      in
      xmm_reg_val_set r [ eax_val; ebx_val; ecx_val; edx_val ] state.xmm_reg_map
    | _ -> error "Movdqa command operand must be a register"
  ;;

  let eval_addpd state = function
    | RegReg (r1, r2) ->
      let* r1_val = xmm_reg_val_get r1 state.xmm_reg_map in
      let* r2_val = xmm_reg_val_get r2 state.xmm_reg_map in
      xmm_reg_val_set r1 (List.map2 ( + ) r1_val r2_val) state.xmm_reg_map
    | _ -> error "Addpd command operands must both be a registers"
  ;;

  let eval_mulpd state = function
    | RegReg (r1, r2) ->
      let* r1_val = xmm_reg_val_get r1 state.xmm_reg_map in
      let* r2_val = xmm_reg_val_get r2 state.xmm_reg_map in
      xmm_reg_val_set r1 (List.map2 ( * ) r1_val r2_val) state.xmm_reg_map
    | _ -> error "Mulpd command operands must both be a registers"
  ;;

  (* Eval B-, W-, D- or Xommand *)
  let eval_bwdxcommand state =
    let eval_helper eval_op x =
      eval_op state.reg_map x >>= fun m -> return { state with reg_map = m }
    in
    let eval_xmm_helper eval_op x =
      eval_op state x >>= fun m -> return { state with xmm_reg_map = m }
    in
    function
    | Mov x -> eval_helper eval_mov x
    | Add x -> eval_helper eval_add x
    | Sub x -> eval_helper eval_sub x
    | Inc x -> eval_helper eval_inc x
    | Mul x -> eval_helper eval_mul x
    | Push x -> eval_push state x
    | Pop x -> eval_pop state x
    | Cmp x -> eval_cmp state.reg_map x >>= fun f -> return { state with flags = f }
    | Movdqa x -> eval_xmm_helper eval_movdqa x
    | Addpd x -> eval_xmm_helper eval_addpd x
    | Mulpd x -> eval_xmm_helper eval_mulpd x
    | _ -> return state
  ;;

  let from_label label_map l =
    match StringMap.find_opt l label_map with
    | None -> error (Printf.sprintf "Label %S not found in the program" l)
    | Some v -> return v
  ;;

  (* Returns tuple of state and list of instructions which we should execute *)
  let eval_scommand state tl = function
    | Jmp (Label l) -> from_label state.label_map l >>= fun i -> return (state, i)
    | Je (Label l) ->
      if state.flags = 0
      then from_label state.label_map l >>= fun i -> return (state, i)
      else return (state, tl)
    | Jne (Label l) ->
      if state.flags <> 0
      then from_label state.label_map l >>= fun i -> return (state, i)
      else return (state, tl)
    | Call (Label l) ->
      from_label state.label_map l
      >>= fun i -> return ({ state with cstack = ListStack.push tl state.cstack }, i)
    | _ -> error "Command not supported"
  ;;

  let eval_ret state =
    match ListStack.peek state.cstack with
    | None -> error "Cannot return from function, the call stack is empty"
    | Some instrs ->
      (match ListStack.pop state.cstack with
       | None ->
         error
           "Critical error: ListStack.pop failed while the previous ListStack.peek \
            succeded"
       | Some s -> return ({ state with cstack = s }, instrs))
  ;;

  let rec eval state = function
    | [] -> return state
    | instr :: tl ->
      (match instr with
       (* We do not care, labels are taken into account in the label_map *)
       | LCommand _ -> eval state tl
       | BCommand Ret -> eval_ret state >>= fun (state, instrs) -> eval state instrs
       | BCommand x -> eval_bwdxcommand state x >>= fun s -> eval s tl
       | WCommand x -> eval_bwdxcommand state x >>= fun s -> eval s tl
       | DCommand x -> eval_bwdxcommand state x >>= fun s -> eval s tl
       | XCommand x -> eval_bwdxcommand state x >>= fun s -> eval s tl
       | SCommand x ->
         eval_scommand state tl x >>= fun (state, instrs) -> eval state instrs)
  ;;

  (* Scan through the AST and check if it contains invalid instructions that are
   prevented by the type system or by parser *)
  let validate_ast program =
    let validate_instr = function
      | BCommand (Inc (Const _)) | WCommand (Inc (Const _)) | DCommand (Inc (Const _)) ->
        error "Inc command operand must be a register"
      | _ -> return ()
    in
    (* List.iter validate_instr program *)
    let rec helper_iter = function
      | [] -> return ()
      | h :: tl -> validate_instr h >>= fun _ -> helper_iter tl
    in
    helper_iter program
  ;;

  let eval_whole whole_program =
    let initial_label_map = gen_label_map whole_program in
    let rec gen_initial_reg_map init map = function
      | [] -> return map
      | h :: tl ->
        reg_name_to_id h >>= fun r -> gen_initial_reg_map init (IntMap.add r init map) tl
    in
    (* Each of dword registers is associated with 0 initial value *)
    let* initial_reg_map = gen_initial_reg_map 0 IntMap.empty dword_reg_name_list in
    let* initial_xmm_reg_map =
      gen_initial_reg_map [ 0; 0; 0; 0 ] IntMap.empty xmm_reg_name_list
    in
    let initial_state =
      { reg_map = initial_reg_map
      ; xmm_reg_map = initial_xmm_reg_map
      ; stack = ListStack.empty
      ; flags = 0
      ; label_map = initial_label_map
      ; cstack = ListStack.empty
      }
    in
    validate_ast whole_program >>= fun _ -> eval initial_state whole_program
  ;;
end

module InterpreterTests = struct
  open Result
  open Interpreter (Result)
  open OperandsHandler.OperandsHandler (Result)
  open Ast.Ast (Result)

  let do_test program check_state =
    match program with
    | Error e ->
      Printf.eprintf "%s" e;
      false
    | Ok program ->
      (match eval_whole program with
       | Error e ->
         Printf.eprintf "Unexpected error occured: %s" e;
         false
       | Ok state -> check_state state)
  ;;

  let%test _ =
    let program =
      let* ax = reg_name_to_word_reg "ax" in
      let* c3 = int_to_word_const 3 in
      let* c2 = int_to_word_const 2 in
      return @@ [ WCommand (Mov (RegConst (ax, c3))); WCommand (Add (RegConst (ax, c2))) ]
    in
    do_test program (fun state ->
      let final_reg_map = state.reg_map in
      match
        let* al = reg_name_to_byte_reg "al" in
        reg_val_get al final_reg_map
      with
      | Error e ->
        Printf.eprintf "%s" e;
        false
      | Ok v -> v = 5)
  ;;

  let%test _ =
    let program =
      let* ah = reg_name_to_byte_reg "ah" in
      let* bl = reg_name_to_byte_reg "bl" in
      let* cl = reg_name_to_byte_reg "cl" in
      let* ax = reg_name_to_word_reg "ax" in
      let* ebx = reg_name_to_dword_reg "ebx" in
      let* ecx = reg_name_to_dword_reg "ecx" in
      let* wc2 = int_to_word_const 2 in
      let* bc3 = int_to_byte_const 3 in
      let* dc7 = int_to_dword_const 7 in
      let* wc8 = int_to_word_const 8 in
      return
      @@ [ LCommand "label"
         ; WCommand (Mov (RegConst (ax, wc8)))
         ; WCommand (Sub (RegConst (ax, wc2)))
         ; DCommand (Mov (RegConst (ebx, dc7)))
         ; BCommand (Add (RegReg (ah, bl)))
         ; WCommand (Inc (Reg ax))
         ; BCommand (Mov (RegConst (cl, bc3)))
         ; DCommand (Mul (Reg ecx))
         ]
    in
    do_test program (fun state ->
      let final_reg_map = state.reg_map in
      match
        let* ax = reg_name_to_word_reg "ax" in
        reg_val_get ax final_reg_map
      with
      | Error e ->
        Printf.eprintf "%s" e;
        false
      | Ok v -> v = ((7 * 256) + 6 + 1) * 3)
  ;;

  let%test _ =
    let program =
      let* ah = reg_name_to_byte_reg "ah" in
      let* bh = reg_name_to_byte_reg "bh" in
      let* bl = reg_name_to_byte_reg "bl" in
      let* cl = reg_name_to_byte_reg "cl" in
      let* ax = reg_name_to_word_reg "ax" in
      let* dx = reg_name_to_word_reg "dx" in
      let* eax = reg_name_to_dword_reg "eax" in
      let* ebx = reg_name_to_dword_reg "ebx" in
      let* ecx = reg_name_to_dword_reg "ecx" in
      let* wc2 = int_to_word_const 2 in
      let* bc3 = int_to_byte_const 3 in
      let* dc7 = int_to_dword_const 7 in
      let* wc8 = int_to_word_const 8 in
      let* bc43 = int_to_byte_const 43 in
      return
      @@ [ LCommand "label"
         ; WCommand (Mov (RegConst (ax, wc8)))
         ; DCommand (Push (Reg eax))
         ; WCommand (Sub (RegConst (ax, wc2)))
         ; DCommand (Mov (RegConst (ebx, dc7)))
         ; BCommand (Push (Const bc43))
         ; BCommand (Add (RegReg (ah, bl)))
         ; WCommand (Inc (Reg ax))
         ; BCommand (Mov (RegConst (cl, bc3)))
         ; WCommand (Pop (Reg dx))
         ; DCommand (Mul (Reg ecx))
         ; BCommand (Pop (Reg bh))
         ]
    in
    do_test program (fun state ->
      let final_reg_map = state.reg_map in
      match
        let* bh = reg_name_to_byte_reg "bh" in
        let* ax = reg_name_to_word_reg "ax" in
        let* edx = reg_name_to_dword_reg "edx" in
        let* bh_val = reg_val_get bh final_reg_map in
        let* ax_val = reg_val_get ax final_reg_map in
        let* edx_val = reg_val_get edx final_reg_map in
        return (bh_val, ax_val, edx_val)
      with
      | Error e ->
        Printf.eprintf "%s" e;
        false
      | Ok v -> v = (8, ((7 * 256) + 6 + 1) * 3, 43))
  ;;

  let%test _ =
    let program =
      let* al = reg_name_to_byte_reg "al" in
      let* ax = reg_name_to_word_reg "ax" in
      let* wc4 = int_to_word_const 4 in
      let* bc5 = int_to_byte_const 5 in
      return
      @@ [ WCommand (Mov (RegConst (ax, wc4))); BCommand (Cmp (RegConst (al, bc5))) ]
    in
    do_test program (fun state -> state.flags < 0)
  ;;

  let%test _ =
    let program =
      let* ax = reg_name_to_word_reg "ax" in
      let* bx = reg_name_to_word_reg "bx" in
      let* cx = reg_name_to_word_reg "cx" in
      let* c1 = int_to_word_const 1 in
      let* c2 = int_to_word_const 2 in
      let* c3 = int_to_word_const 3 in
      let* c4 = int_to_word_const 4 in
      return
      @@ [ WCommand (Mov (RegConst (ax, c1)))
         ; SCommand (Jmp (Label "l1"))
         ; WCommand (Mov (RegConst (ax, c2)))
         ; LCommand "l1"
         ; WCommand (Mov (RegConst (bx, c1)))
         ; WCommand (Cmp (RegReg (ax, bx)))
         ; SCommand (Je (Label "l2"))
         ; WCommand (Mov (RegConst (cx, c3)))
         ; SCommand (Jmp (Label "exit"))
         ; LCommand "l2"
         ; WCommand (Mov (RegConst (cx, c4)))
         ; LCommand "exit"
         ]
    in
    do_test program (fun state ->
      let final_reg_map = state.reg_map in
      match
        let* cx = reg_name_to_word_reg "cx" in
        reg_val_get cx final_reg_map
      with
      | Error e ->
        Printf.eprintf "%s" e;
        false
      | Ok v -> v = 4)
  ;;

  let%test _ =
    let program =
      let* ax = reg_name_to_word_reg "ax" in
      let* bx = reg_name_to_word_reg "bx" in
      let* cx = reg_name_to_word_reg "cx" in
      let* c1 = int_to_word_const 1 in
      let* c2 = int_to_word_const 2 in
      let* c3 = int_to_word_const 3 in
      let* c4 = int_to_word_const 4 in
      return
      @@ [ WCommand (Mov (RegConst (ax, c1)))
         ; SCommand (Jmp (Label "l1"))
         ; WCommand (Mov (RegConst (ax, c2)))
         ; LCommand "l1"
         ; WCommand (Mov (RegConst (bx, c1)))
         ; WCommand (Cmp (RegReg (ax, bx)))
         ; SCommand (Jne (Label "l2"))
         ; WCommand (Mov (RegConst (cx, c3)))
         ; SCommand (Jmp (Label "exit"))
         ; LCommand "l2"
         ; WCommand (Mov (RegConst (cx, c4)))
         ; LCommand "exit"
         ]
    in
    do_test program (fun state ->
      let final_reg_map = state.reg_map in
      match
        let* cx = reg_name_to_word_reg "cx" in
        reg_val_get cx final_reg_map
      with
      | Error e ->
        Printf.eprintf "%s" e;
        false
      | Ok v -> v = 3)
  ;;

  let%test _ =
    let program =
      let* ax = reg_name_to_word_reg "ax" in
      let* bx = reg_name_to_word_reg "bx" in
      let* cx = reg_name_to_word_reg "cx" in
      let* c1 = int_to_word_const 1 in
      let* c2 = int_to_word_const 2 in
      let* c3 = int_to_word_const 3 in
      let* c4 = int_to_word_const 4 in
      let* c43 = int_to_word_const 43 in
      return
      @@ [ WCommand (Mov (RegConst (ax, c1)))
         ; SCommand (Jmp (Label "l1"))
         ; WCommand (Mov (RegConst (ax, c2)))
         ; LCommand "l1"
         ; WCommand (Mov (RegConst (bx, c43)))
         ; WCommand (Cmp (RegReg (ax, bx)))
         ; SCommand (Je (Label "l2"))
         ; WCommand (Mov (RegConst (cx, c3)))
         ; SCommand (Jmp (Label "exit"))
         ; LCommand "l2"
         ; WCommand (Mov (RegConst (cx, c4)))
         ; LCommand "exit"
         ]
    in
    do_test program (fun state ->
      let final_reg_map = state.reg_map in
      match
        let* cx = reg_name_to_word_reg "cx" in
        reg_val_get cx final_reg_map
      with
      | Error e ->
        Printf.eprintf "%s" e;
        false
      | Ok v -> v = 3)
  ;;

  let%test _ =
    let program =
      let* eax = reg_name_to_dword_reg "eax" in
      let* ebx = reg_name_to_dword_reg "ebx" in
      let* ecx = reg_name_to_dword_reg "ecx" in
      let* c0 = int_to_dword_const 0 in
      let* c1 = int_to_dword_const 1 in
      let* c2 = int_to_dword_const 2 in
      let* c9 = int_to_dword_const 9 in
      return
      @@ [ DCommand (Mov (RegConst (eax, c9)))
         ; SCommand (Call (Label "fib"))
         ; SCommand (Jmp (Label "end"))
         ; LCommand "fib"
         ; DCommand (Cmp (RegConst (eax, c0)))
         ; SCommand (Jne (Label "l1"))
         ; DCommand (Mov (RegConst (ebx, c0)))
         ; BCommand Ret
         ; LCommand "l1"
         ; DCommand (Cmp (RegConst (eax, c1)))
         ; SCommand (Jne (Label "l2"))
         ; DCommand (Mov (RegConst (ebx, c1)))
         ; BCommand Ret
         ; LCommand "l2"
         ; DCommand (Push (Reg eax))
         ; DCommand (Sub (RegConst (eax, c2)))
         ; SCommand (Call (Label "fib"))
         ; DCommand (Pop (Reg eax))
         ; DCommand (Push (Reg ebx))
         ; DCommand (Sub (RegConst (eax, c1)))
         ; SCommand (Call (Label "fib"))
         ; DCommand (Pop (Reg ecx))
         ; DCommand (Add (RegReg (ebx, ecx)))
         ; BCommand Ret
         ; LCommand "end"
         ]
    in
    do_test program (fun state ->
      let final_reg_map = state.reg_map in
      match
        let* ebx = reg_name_to_dword_reg "ebx" in
        reg_val_get ebx final_reg_map
      with
      | Error e ->
        Printf.eprintf "%s" e;
        false
      | Ok v -> v = 34)
  ;;

  let%test _ =
    let program =
      let* eax = reg_name_to_dword_reg "eax" in
      let* ebx = reg_name_to_dword_reg "ebx" in
      let* ecx = reg_name_to_dword_reg "ecx" in
      let* edx = reg_name_to_dword_reg "edx" in
      let* xmm0 = reg_name_to_xmm_reg "xmm0" in
      let* xmm7 = reg_name_to_xmm_reg "xmm7" in
      let* c1 = int_to_dword_const 1 in
      let* c2 = int_to_dword_const 2 in
      let* c3 = int_to_dword_const 3 in
      let* c4 = int_to_dword_const 4 in
      let* c5 = int_to_dword_const 5 in
      return
      @@ [ DCommand (Mov (RegConst (eax, c1)))
         ; DCommand (Mov (RegConst (ebx, c2)))
         ; DCommand (Mov (RegConst (ecx, c3)))
         ; DCommand (Mov (RegConst (edx, c4)))
         ; XCommand (Movdqa (Reg xmm0))
         ; DCommand (Mov (RegConst (eax, c5)))
         ; XCommand (Movdqa (Reg xmm7))
         ; XCommand (Addpd (RegReg (xmm0, xmm7)))
         ]
    in
    do_test program (fun state ->
      let final_xmm_reg_map = state.xmm_reg_map in
      match
        let* xmm0 = reg_name_to_xmm_reg "xmm0" in
        xmm_reg_val_get xmm0 final_xmm_reg_map
      with
      | Error e ->
        Printf.eprintf "%s" e;
        false
      | Ok v -> List.equal ( = ) v [ 6; 4; 6; 8 ])
  ;;

  (* Calculate (1, 2, 3) x ((4, 5, 6), (7, 8, 9), (10, 11, 12)).
     The answer is located in 3 first dwords of xmm0 *)
  let%test _ =
    let program =
      let* eax = reg_name_to_dword_reg "eax" in
      let* ebx = reg_name_to_dword_reg "ebx" in
      let* ecx = reg_name_to_dword_reg "ecx" in
      let* xmm0 = reg_name_to_xmm_reg "xmm0" in
      let* xmm1 = reg_name_to_xmm_reg "xmm1" in
      let* xmm2 = reg_name_to_xmm_reg "xmm2" in
      let* xmm3 = reg_name_to_xmm_reg "xmm3" in
      let* xmm4 = reg_name_to_xmm_reg "xmm4" in
      let* xmm5 = reg_name_to_xmm_reg "xmm5" in
      let* c1 = int_to_dword_const 1 in
      let* c2 = int_to_dword_const 2 in
      let* c3 = int_to_dword_const 3 in
      let* c4 = int_to_dword_const 4 in
      let* c5 = int_to_dword_const 5 in
      let* c6 = int_to_dword_const 6 in
      let* c7 = int_to_dword_const 7 in
      let* c8 = int_to_dword_const 8 in
      let* c9 = int_to_dword_const 9 in
      let* c10 = int_to_dword_const 10 in
      let* c11 = int_to_dword_const 11 in
      let* c12 = int_to_dword_const 12 in
      return
      @@ [ DCommand (Mov (RegConst (eax, c1)))
         ; DCommand (Mov (RegConst (ebx, c1)))
         ; DCommand (Mov (RegConst (ecx, c1)))
         ; XCommand (Movdqa (Reg xmm0))
         ; DCommand (Mov (RegConst (eax, c2)))
         ; DCommand (Mov (RegConst (ebx, c2)))
         ; DCommand (Mov (RegConst (ecx, c2)))
         ; XCommand (Movdqa (Reg xmm1))
         ; DCommand (Mov (RegConst (eax, c3)))
         ; DCommand (Mov (RegConst (ebx, c3)))
         ; DCommand (Mov (RegConst (ecx, c3)))
         ; XCommand (Movdqa (Reg xmm2))
         ; DCommand (Mov (RegConst (eax, c4)))
         ; DCommand (Mov (RegConst (ebx, c5)))
         ; DCommand (Mov (RegConst (ecx, c6)))
         ; XCommand (Movdqa (Reg xmm3))
         ; DCommand (Mov (RegConst (eax, c7)))
         ; DCommand (Mov (RegConst (ebx, c8)))
         ; DCommand (Mov (RegConst (ecx, c9)))
         ; XCommand (Movdqa (Reg xmm4))
         ; DCommand (Mov (RegConst (eax, c10)))
         ; DCommand (Mov (RegConst (ebx, c11)))
         ; DCommand (Mov (RegConst (ecx, c12)))
         ; XCommand (Movdqa (Reg xmm5))
         ; XCommand (Mulpd (RegReg (xmm0, xmm3)))
         ; XCommand (Mulpd (RegReg (xmm1, xmm4)))
         ; XCommand (Mulpd (RegReg (xmm2, xmm5)))
         ; XCommand (Addpd (RegReg (xmm0, xmm1)))
         ; XCommand (Addpd (RegReg (xmm0, xmm2)))
         ]
    in
    do_test program (fun state ->
      let final_xmm_reg_map = state.xmm_reg_map in
      match
        let* xmm0 = reg_name_to_xmm_reg "xmm0" in
        xmm_reg_val_get xmm0 final_xmm_reg_map
      with
      | Error e ->
        Printf.eprintf "%s" e;
        false
      | Ok v -> List.equal ( = ) v [ 48; 54; 60; 0 ])
  ;;

  let%test _ =
    let program =
      let* c7 = int_to_word_const 7 in
      return @@ [ WCommand (Inc (Const c7)) ]
    in
    match program >>= fun program -> eval_whole program with
    | Error _ -> true
    | Ok _ ->
      Printf.eprintf "An error was expected but it didn't occur";
      false
  ;;
end
