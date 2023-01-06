(** Copyright 2021-2023, andreyizrailev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open MonadError
open Utils

module Transformations (M : MonadError) = struct
  open OperandsHandler.OperandsHandler (M)
  open Ast.Ast (M)
  open M

  (* Make all registers related to [reg] invalid *)
  let invalidate reg_map reg =
    let related_regs = reg_get_related_regs reg in
    List.fold_left (fun map r -> IntMap.add r None map) reg_map related_regs
  ;;

  let invalidate_all reg_map =
    IntMap.fold
      (fun r _ new_reg_map -> IntMap.add r None new_reg_map)
      reg_map
      IntMap.empty
  ;;

  let process_mov reg_map = function
    | RegConst (r, c) ->
      let new_reg_map =
        invalidate reg_map r |> IntMap.add (reg_to_id r) (Some (const_val c))
      in
      new_reg_map, RegConst (r, c)
    | RegReg (r1, r2) ->
      let new_reg_map = invalidate reg_map r1 in
      (match IntMap.find (reg_to_id r2) new_reg_map with
       (* We know nothing about r2, so we can't substitute it or r1 with a constant *)
       | None -> new_reg_map, RegReg (r1, r2)
       | Some v ->
         let new_reg_map = IntMap.add (reg_to_id r1) (Some v) new_reg_map in
         new_reg_map, RegConst (r1, const_from_val v))
  ;;

  let process_add reg_map = function
    | RegConst (r, c) ->
      (match IntMap.find (reg_to_id r) reg_map with
       | None ->
         let new_reg_map = invalidate reg_map r in
         new_reg_map, RegConst (r, c)
       | Some v ->
         let new_reg_map = IntMap.add (reg_to_id r) (Some (v + const_val c)) reg_map in
         new_reg_map, RegConst (r, c))
    | RegReg (r1, r2) ->
      (match IntMap.find (reg_to_id r2) reg_map with
       | None ->
         let new_reg_map = invalidate reg_map r1 in
         new_reg_map, RegReg (r1, r2)
       | Some v2 ->
         (match IntMap.find (reg_to_id r1) reg_map with
          | None ->
            let new_reg_map = invalidate reg_map r1 in
            new_reg_map, RegConst (r1, const_from_val v2)
          | Some v1 ->
            let new_reg_map = IntMap.add (reg_to_id r1) (Some (v1 + v2)) reg_map in
            new_reg_map, RegConst (r1, const_from_val v2)))
  ;;

  let process_sub reg_map = function
    | RegConst (r, c) ->
      (match IntMap.find (reg_to_id r) reg_map with
       | None ->
         let new_reg_map = invalidate reg_map r in
         new_reg_map, RegConst (r, c)
       | Some v ->
         let new_reg_map = IntMap.add (reg_to_id r) (Some (v - const_val c)) reg_map in
         new_reg_map, RegConst (r, c))
    | RegReg (r1, r2) ->
      (match IntMap.find (reg_to_id r2) reg_map with
       | None ->
         let new_reg_map = invalidate reg_map r1 in
         new_reg_map, RegReg (r1, r2)
       | Some v2 ->
         (match IntMap.find (reg_to_id r1) reg_map with
          | None ->
            let new_reg_map = invalidate reg_map r1 in
            new_reg_map, RegConst (r1, const_from_val v2)
          | Some v1 ->
            let new_reg_map = IntMap.add (reg_to_id r1) (Some (v1 - v2)) reg_map in
            new_reg_map, RegConst (r1, const_from_val v2)))
  ;;

  let process_inc reg_map = function
    | Reg r ->
      (match IntMap.find (reg_to_id r) reg_map with
       | None -> invalidate reg_map r
       | Some v -> IntMap.add (reg_to_id r) (Some (v + 1)) reg_map)
    | _ -> reg_map
  ;;

  let process_mul reg_map = function
    | Const c ->
      (match IntMap.find reg_eax_id reg_map with
       | None ->
         let new_reg_map = invalidate reg_map reg_eax in
         new_reg_map, Const c
       | Some v ->
         let new_reg_map = IntMap.add reg_eax_id (Some (v * const_val c)) reg_map in
         new_reg_map, Const c)
    | Reg r ->
      (match IntMap.find (reg_to_id r) reg_map with
       | None ->
         let new_reg_map = invalidate reg_map reg_eax in
         new_reg_map, Reg r
       | Some v2 ->
         (match IntMap.find reg_eax_id reg_map with
          | None ->
            let new_reg_map = invalidate reg_map reg_eax in
            new_reg_map, Const (const_from_val v2)
          | Some v1 ->
            let new_reg_map = IntMap.add reg_eax_id (Some (v1 * v2)) reg_map in
            new_reg_map, Const (const_from_val v2)))
    | _ as operand -> reg_map, operand
  ;;

  let process_push reg_map = function
    | Reg r ->
      (match IntMap.find (reg_to_id r) reg_map with
       | None -> reg_map, Reg r
       | Some v -> reg_map, Const (const_from_val v))
    | _ as operand -> reg_map, operand
  ;;

  let process_pop reg_map = function
    | Reg r -> invalidate reg_map r
    | _ -> reg_map
  ;;

  let transform_instr reg_map = function
    (*********************    Mov    ***************************)
    | BCommand (Mov x) ->
      let new_reg_map, new_operand = process_mov reg_map x in
      new_reg_map, BCommand (Mov new_operand)
    | WCommand (Mov x) ->
      let new_reg_map, new_operand = process_mov reg_map x in
      new_reg_map, WCommand (Mov new_operand)
    | DCommand (Mov x) ->
      let new_reg_map, new_operand = process_mov reg_map x in
      new_reg_map, DCommand (Mov new_operand)
    (*********************    Add    ***************************)
    | BCommand (Add x) ->
      let new_reg_map, new_operand = process_add reg_map x in
      new_reg_map, BCommand (Add new_operand)
    | WCommand (Add x) ->
      let new_reg_map, new_operand = process_add reg_map x in
      new_reg_map, WCommand (Add new_operand)
    | DCommand (Add x) ->
      let new_reg_map, new_operand = process_add reg_map x in
      new_reg_map, DCommand (Add new_operand)
    (*********************    Sub    ***************************)
    | BCommand (Sub x) ->
      let new_reg_map, new_operand = process_sub reg_map x in
      new_reg_map, BCommand (Sub new_operand)
    | WCommand (Sub x) ->
      let new_reg_map, new_operand = process_sub reg_map x in
      new_reg_map, WCommand (Sub new_operand)
    | DCommand (Sub x) ->
      let new_reg_map, new_operand = process_sub reg_map x in
      new_reg_map, DCommand (Sub new_operand)
    (*********************    Inc    ***************************)
    | BCommand (Inc x) as instr ->
      let new_reg_map = process_inc reg_map x in
      new_reg_map, instr
    | WCommand (Inc x) as instr ->
      let new_reg_map = process_inc reg_map x in
      new_reg_map, instr
    | DCommand (Inc x) as instr ->
      let new_reg_map = process_inc reg_map x in
      new_reg_map, instr
    (*********************    Mul    ***************************)
    | BCommand (Mul x) ->
      let new_reg_map, new_operand = process_mul reg_map x in
      new_reg_map, BCommand (Mul new_operand)
    | WCommand (Mul x) ->
      let new_reg_map, new_operand = process_mul reg_map x in
      new_reg_map, WCommand (Mul new_operand)
    | DCommand (Mul x) ->
      let new_reg_map, new_operand = process_mul reg_map x in
      new_reg_map, DCommand (Mul new_operand)
    (*********************    Push   ***************************)
    | BCommand (Push x) ->
      let new_reg_map, new_operand = process_push reg_map x in
      new_reg_map, BCommand (Push new_operand)
    | WCommand (Push x) ->
      let new_reg_map, new_operand = process_push reg_map x in
      new_reg_map, WCommand (Push new_operand)
    | DCommand (Push x) ->
      let new_reg_map, new_operand = process_push reg_map x in
      new_reg_map, DCommand (Push new_operand)
    (*********************    Pop    ***************************)
    | BCommand (Pop x) as instr ->
      let new_reg_map = process_pop reg_map x in
      new_reg_map, instr
    | WCommand (Pop x) as instr ->
      let new_reg_map = process_pop reg_map x in
      new_reg_map, instr
    | DCommand (Pop x) as instr ->
      let new_reg_map = process_pop reg_map x in
      new_reg_map, instr
    (*********************    Labels    ************************)
    | LCommand _ as instr ->
      let new_reg_map = invalidate_all reg_map in
      new_reg_map, instr
    (*********************    Other    *************************)
    | _ as instr -> reg_map, instr
  ;;

  let rec transform reg_map new_program = function
    | [] -> new_program
    | instr :: tl ->
      let new_reg_map, new_instr = transform_instr reg_map instr in
      transform new_reg_map (new_program @ [ new_instr ]) tl
  ;;

  let transform_whole whole_program =
    let reg_name_list = byte_reg_name_list @ word_reg_name_list @ dword_reg_name_list in
    let rec gen_initial_reg_map map = function
      | [] -> return map
      | h :: tl ->
        reg_name_to_id h >>= fun r -> gen_initial_reg_map (IntMap.add r None map) tl
    in
    let* initial_reg_map = gen_initial_reg_map IntMap.empty reg_name_list in
    return @@ transform initial_reg_map [] whole_program
  ;;
end

module TransformationsTest = struct
  open Result
  open Transformations (Result)
  open OperandsHandler.OperandsHandler (Result)
  open Ast.Ast (Result)

  let do_test input expected =
    match
      let* output = transform_whole input in
      return (List.equal (fun a b -> compare a b = 0) output expected)
    with
    | Error e ->
      Printf.eprintf "%s" e;
      false
    | Ok v -> v
  ;;

  let%test _ =
    match
      let* ax = reg_name_to_word_reg "ax" in
      let* bx = reg_name_to_word_reg "bx" in
      let* c3 = int_to_word_const 3 in
      let* input =
        return @@ [ WCommand (Mov (RegConst (ax, c3))); WCommand (Mov (RegReg (bx, ax))) ]
      in
      let* expected =
        return
        @@ [ WCommand (Mov (RegConst (ax, c3))); WCommand (Mov (RegConst (bx, c3))) ]
      in
      return @@ do_test input expected
    with
    | Error e ->
      Printf.eprintf "%s" e;
      false
    | Ok v -> v
  ;;

  let%test _ =
    match
      let* ax = reg_name_to_word_reg "ax" in
      let* bx = reg_name_to_word_reg "bx" in
      let* c2 = int_to_word_const 2 in
      let* c3 = int_to_word_const 3 in
      let* c5 = int_to_word_const 5 in
      let* input =
        return
        @@ [ WCommand (Mov (RegConst (ax, c3)))
           ; WCommand (Add (RegConst (ax, c2)))
           ; WCommand (Mov (RegReg (bx, ax)))
           ]
      in
      let* expected =
        return
        @@ [ WCommand (Mov (RegConst (ax, c3)))
           ; WCommand (Add (RegConst (ax, c2)))
           ; WCommand (Mov (RegConst (bx, c5)))
           ]
      in
      return @@ do_test input expected
    with
    | Error e ->
      Printf.eprintf "%s" e;
      false
    | Ok v -> v
  ;;

  let%test _ =
    match
      let* ax = reg_name_to_word_reg "ax" in
      let* bx = reg_name_to_word_reg "bx" in
      let* cx = reg_name_to_word_reg "cx" in
      let* c1 = int_to_word_const 1 in
      let* c2 = int_to_word_const 2 in
      let* c3 = int_to_word_const 3 in
      let* c4 = int_to_word_const 4 in
      let* input =
        return
        @@ [ WCommand (Mov (RegConst (ax, c3)))
           ; WCommand (Add (RegConst (ax, c2)))
           ; WCommand (Mov (RegConst (cx, c1)))
           ; WCommand (Sub (RegReg (ax, cx)))
           ; WCommand (Mov (RegReg (bx, ax)))
           ]
      in
      let* expected =
        return
        @@ [ WCommand (Mov (RegConst (ax, c3)))
           ; WCommand (Add (RegConst (ax, c2)))
           ; WCommand (Mov (RegConst (cx, c1)))
           ; WCommand (Sub (RegConst (ax, c1)))
           ; WCommand (Mov (RegConst (bx, c4)))
           ]
      in
      return @@ do_test input expected
    with
    | Error e ->
      Printf.eprintf "%s" e;
      false
    | Ok v -> v
  ;;

  let%test _ =
    match
      let* ax = reg_name_to_word_reg "ax" in
      let* bx = reg_name_to_word_reg "bx" in
      let* c2 = int_to_word_const 2 in
      let* c3 = int_to_word_const 3 in
      let* input =
        return
        @@ [ WCommand (Mov (RegConst (ax, c3)))
           ; WCommand (Add (RegConst (ax, c2)))
           ; LCommand "label"
           ; WCommand (Mov (RegReg (bx, ax)))
           ]
      in
      return @@ do_test input input
    with
    | Error e ->
      Printf.eprintf "%s" e;
      false
    | Ok v -> v
  ;;

  let%test _ =
    match
      let* ax = reg_name_to_word_reg "ax" in
      let* bx = reg_name_to_word_reg "bx" in
      let* cx = reg_name_to_word_reg "cx" in
      let* c2 = int_to_word_const 2 in
      let* c3 = int_to_word_const 3 in
      let* input =
        return
        @@ [ WCommand (Mov (RegConst (ax, c3)))
           ; WCommand (Add (RegConst (ax, c2)))
             (* We add something unknown to ax so it is invalidated *)
           ; WCommand (Add (RegReg (ax, cx)))
           ; WCommand (Mov (RegReg (bx, ax)))
           ]
      in
      return @@ do_test input input
    with
    | Error e ->
      Printf.eprintf "%s" e;
      false
    | Ok v -> v
  ;;

  let%test _ =
    match
      let* ax = reg_name_to_word_reg "ax" in
      let* bx = reg_name_to_word_reg "bx" in
      let* al = reg_name_to_byte_reg "al" in
      let* dh = reg_name_to_byte_reg "dh" in
      let* c2 = int_to_word_const 2 in
      let* c3 = int_to_word_const 3 in
      let* c43 = int_to_byte_const 43 in
      let* c44 = int_to_byte_const 44 in
      let* input =
        return
        @@ [ WCommand (Mov (RegConst (ax, c3)))
           ; WCommand (Add (RegConst (ax, c2))) (* Writing to al also invalidates ax *)
           ; BCommand (Mov (RegConst (al, c43)))
           ; WCommand (Mov (RegReg (bx, ax)))
           ; BCommand (Mov (RegReg (dh, al))) (* But now al is substituted *)
           ; BCommand (Inc (Reg dh))
           ; BCommand (Mov (RegReg (al, dh)))
           ]
      in
      let* expected =
        return
        @@ [ WCommand (Mov (RegConst (ax, c3)))
           ; WCommand (Add (RegConst (ax, c2)))
           ; BCommand (Mov (RegConst (al, c43)))
           ; WCommand (Mov (RegReg (bx, ax)))
           ; BCommand (Mov (RegConst (dh, c43)))
           ; BCommand (Inc (Reg dh))
           ; BCommand (Mov (RegConst (al, c44)))
           ]
      in
      return @@ do_test input expected
    with
    | Error e ->
      Printf.eprintf "%s" e;
      false
    | Ok v -> v
  ;;
end
