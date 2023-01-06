(** Copyright 2021-2023, andreyizrailev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open MonadError

module OperandsHandler (M : MonadError) = struct
  open Base
  open Utils
  open M

  type byte
  type word
  type dword
  type xmm

  let pp_byte fmt _ = Format.fprintf fmt "byte"
  let pp_word fmt _ = Format.fprintf fmt "word"
  let pp_dword fmt _ = Format.fprintf fmt "dword"
  let pp_xmm fmt _ = Format.fprintf fmt "xmm"

  type 'a reg = int [@@deriving show { with_path = false }]
  type 'a const = int [@@deriving show { with_path = false }]

  let int_is_byte_const x = x >= -(2 ** 7) && x <= (2 ** 7) - 1
  let int_is_word_const x = x >= -(2 ** 15) && x <= (2 ** 15) - 1
  let int_is_dword_const x = x >= -(2 ** 31) && x <= (2 ** 31) - 1

  (* Though the following three function may look strange, they copy NASM's behaviour *)
  let int_to_byte_const x =
    if not (int_is_byte_const x)
    then error "Int8 expected"
    else if x < 0
    then return 0
    else return x
  ;;

  let int_to_word_const x =
    if not (int_is_word_const x)
    then error "Int16 expected"
    else if x < 0
    then return 0
    else return x
  ;;

  let int_to_dword_const x =
    if not (int_is_dword_const x)
    then error "Int32 expected"
    else if x < 0
    then return 0
    else return x
  ;;

  (* Lists of register names *)
  let byte_reg_name_list = [ "ah"; "al"; "bh"; "bl"; "ch"; "cl"; "dh"; "dl" ]
  let word_reg_name_list = [ "ax"; "bx"; "cx"; "dx" ]
  let dword_reg_name_list = [ "eax"; "ebx"; "ecx"; "edx" ]

  let xmm_reg_name_list =
    [ "xmm0"; "xmm1"; "xmm2"; "xmm3"; "xmm4"; "xmm5"; "xmm6"; "xmm7" ]
  ;;

  (* Lengths of each of the lists just to avoid calculating them everytime we need them *)
  let byte_reg_list_len = List.length byte_reg_name_list
  let word_reg_list_len = List.length word_reg_name_list
  let dword_reg_list_len = List.length dword_reg_name_list
  let xmm_reg_list_len = List.length xmm_reg_name_list

  (* Functions to check if the id belongs to a register of particular size *)
  let reg_id_is_byte_reg reg_id = reg_id < byte_reg_list_len

  let reg_id_is_word_reg reg_id =
    reg_id >= byte_reg_list_len && reg_id < byte_reg_list_len + word_reg_list_len
  ;;

  let reg_id_is_dword_reg reg_id =
    reg_id >= byte_reg_list_len + word_reg_list_len
    && reg_id < byte_reg_list_len + word_reg_list_len + dword_reg_list_len
  ;;

  let reg_id_is_xmm_reg reg_id =
    reg_id >= byte_reg_list_len + word_reg_list_len + dword_reg_list_len
  ;;

  (* Convert register id to 'a reg *)
  let int_to_byte_reg reg_id =
    if reg_id_is_byte_reg reg_id
    then return reg_id
    else error (Printf.sprintf "Register with id %d is not a byte register" reg_id)
  ;;

  let int_to_word_reg reg_id =
    if reg_id_is_word_reg reg_id
    then return reg_id
    else error (Printf.sprintf "Register with id %d is not a word register" reg_id)
  ;;

  let int_to_dword_reg reg_id =
    if reg_id_is_dword_reg reg_id
    then return reg_id
    else error (Printf.sprintf "Register with id %d is not a dword register" reg_id)
  ;;

  let int_to_xmm_reg reg_id =
    if reg_id_is_xmm_reg reg_id
    then return reg_id
    else error (Printf.sprintf "Register with id %d is not an xmm register" reg_id)
  ;;

  let all_reg_name_list =
    byte_reg_name_list @ word_reg_name_list @ dword_reg_name_list @ xmm_reg_name_list
  ;;

  let reg_to_id : 'a reg -> int = Fun.id
  let const_val : 'a const -> int = Fun.id
  let const_from_val : int -> 'a const = Fun.id
  let reg_eax_id = byte_reg_list_len + word_reg_list_len
  let reg_eax = reg_eax_id

  let reg_name_to_id reg_name =
    match List.index_of_elem reg_name String.equal all_reg_name_list with
    | None -> error (Printf.sprintf "No register called %S" reg_name)
    | Some x -> return x
  ;;

  let reg_name_to_byte_reg reg_name = reg_name_to_id reg_name >>= int_to_byte_reg
  let reg_name_to_word_reg reg_name = reg_name_to_id reg_name >>= int_to_word_reg
  let reg_name_to_dword_reg reg_name = reg_name_to_id reg_name >>= int_to_dword_reg
  let reg_name_to_xmm_reg reg_name = reg_name_to_id reg_name >>= int_to_xmm_reg

  (* Get id of a corresponding primary register, e.g. for id of "ch" this
     function returns id of "ecx" *)
  let reg_get_primary_id reg =
    let reg_id = reg_to_id reg in
    if reg_id_is_dword_reg reg_id
    then reg_id
    else if reg_id_is_word_reg reg_id
    then reg_id + word_reg_list_len
    else (reg_id / 2) + byte_reg_list_len + word_reg_list_len
  ;;

  let reg_get_related_regs reg =
    let primary_id = reg_get_primary_id reg in
    let ex = primary_id in
    let x = primary_id - word_reg_list_len in
    let h = (primary_id - byte_reg_list_len - word_reg_list_len) * 2 in
    let l = h + 1 in
    [ ex; x; h; l ]
  ;;

  let reg_val_get reg reg_map =
    let reg_id = reg_to_id reg in
    let primary_id = reg_get_primary_id reg_id in
    let primary_val = IntMap.find primary_id reg_map in
    return
    @@
    if reg_id_is_dword_reg reg_id
    then primary_val
    else if reg_id_is_word_reg reg_id
    then primary_val land 0xFFFF
    else if reg_id % 2 = 1 (* Our register is like "al" *)
    then primary_val land 0xFF
    else (primary_val land 0xFF00) lsr 8
  ;;

  let reg_val_set reg value reg_map =
    let reg_id = reg_to_id reg in
    let primary_id = reg_get_primary_id reg_id in
    let primary_value = IntMap.find primary_id reg_map in
    let value_to_set =
      if reg_id_is_dword_reg reg_id
      then value land 0xFFFFFFFF
      else if reg_id_is_word_reg reg_id
      then ((primary_value lsr 16) lsl 16) lor (value land 0xFFFF)
      else if reg_id % 2 = 1
      then ((primary_value lsr 8) lsl 8) lor (value land 0xFF)
      else
        ((primary_value lsr 16) lsl 16)
        lor (((value land 0xFF) lsl 8) lor (primary_value land 0xFF))
    in
    return @@ IntMap.add primary_id value_to_set reg_map
  ;;

  let xmm_reg_val_get xmm_reg xmm_reg_map =
    let xmm_reg_id = reg_to_id xmm_reg in
    return @@ IntMap.find xmm_reg_id xmm_reg_map
  ;;

  let xmm_reg_val_set xmm_reg xmm_value xmm_reg_map =
    let validate =
      if List.length xmm_value != 4
      then error "Value of an xmm register must be a list of 4 integers"
      else (
        let rec helper_iter = function
          | [] -> return ()
          | h :: tl ->
            if not (int_is_dword_const h)
            then error "All elements of xmm register value list must be 32-bit long"
            else helper_iter tl
        in
        helper_iter xmm_value)
    in
    let xmm_reg_id = reg_to_id xmm_reg in
    validate >>= fun _ -> return @@ IntMap.add xmm_reg_id xmm_value xmm_reg_map
  ;;
end

module RegMapTest = struct
  open Utils.IntMap
  open OperandsHandler (Result)
  open Result

  let do_test lhs rhs =
    match lhs >>= fun l -> rhs >>= fun r -> return (l = r) with
    | Error e ->
      Printf.eprintf "%s" e;
      false
    | Ok v -> v
  ;;

  let%test _ =
    do_test
      (reg_name_to_dword_reg "ecx" >>= fun r -> return (reg_get_primary_id r))
      (reg_name_to_dword_reg "ecx")
  ;;

  let%test _ =
    do_test
      (reg_name_to_word_reg "bx" >>= fun r -> return (reg_get_primary_id r))
      (reg_name_to_dword_reg "ebx")
  ;;

  let%test _ =
    do_test
      (reg_name_to_byte_reg "dh" >>= fun r -> return (reg_get_primary_id r))
      (reg_name_to_dword_reg "edx")
  ;;

  let%test _ =
    do_test
      (reg_name_to_byte_reg "al" >>= fun r -> return (reg_get_primary_id r))
      (reg_name_to_dword_reg "eax")
  ;;

  let%test _ =
    let expr =
      let* ecx = reg_name_to_dword_reg "ecx" in
      let* cx = reg_name_to_word_reg "cx" in
      let* ch = reg_name_to_byte_reg "ch" in
      let* cl = reg_name_to_byte_reg "cl" in
      return @@ (reg_get_related_regs ch, [ ecx; cx; ch; cl ])
    in
    match expr with
    | Error e ->
      Printf.eprintf "%s" e;
      false
    | Ok (l1, l2) -> List.equal ( = ) l1 l2
  ;;

  let reg_map =
    reg_name_to_dword_reg "eax"
    >>= fun eax ->
    reg_name_to_dword_reg "ebx"
    >>= fun ebx ->
    reg_name_to_dword_reg "ecx"
    >>= fun ecx ->
    reg_name_to_dword_reg "edx"
    >>= fun edx ->
    return (empty |> add eax 0xABCD1234 |> add ebx 0 |> add ecx 0 |> add edx 0)
  ;;

  let%test _ =
    do_test
      (reg_name_to_dword_reg "eax"
      >>= fun r -> reg_map >>= fun reg_map -> reg_val_get r reg_map)
      (return 0xABCD1234)
  ;;

  let%test _ =
    do_test
      (reg_name_to_word_reg "ax"
      >>= fun r -> reg_map >>= fun reg_map -> reg_val_get r reg_map)
      (return 0x1234)
  ;;

  let%test _ =
    do_test
      (reg_name_to_byte_reg "al"
      >>= fun r -> reg_map >>= fun reg_map -> reg_val_get r reg_map)
      (return 0x34)
  ;;

  let%test _ =
    do_test
      (reg_name_to_byte_reg "ah"
      >>= fun r -> reg_map >>= fun reg_map -> reg_val_get r reg_map)
      (return 0x12)
  ;;

  let reg_map =
    reg_name_to_dword_reg "ecx"
    >>= fun ecx -> reg_map >>= fun reg_map -> return (add ecx 0xABCD1234 reg_map)
  ;;

  let%test _ =
    do_test
      (reg_name_to_byte_reg "ch"
      >>= fun ch ->
      reg_map
      >>= fun reg_map ->
      reg_val_set ch 0x10 reg_map
      >>= fun reg_map ->
      reg_name_to_dword_reg "ecx" >>= fun ecx -> reg_val_get ecx reg_map)
      (return 0xABCD1034)
  ;;

  let%test _ =
    do_test
      (reg_name_to_byte_reg "cl"
      >>= fun cl ->
      reg_map
      >>= fun reg_map ->
      reg_val_set cl 0x0 reg_map
      >>= fun reg_map ->
      reg_name_to_dword_reg "ecx" >>= fun ecx -> reg_val_get ecx reg_map)
      (return 0xABCD1200)
  ;;

  let%test _ =
    do_test
      (reg_name_to_word_reg "cx"
      >>= fun cx ->
      reg_map
      >>= fun reg_map ->
      reg_val_set cx 0x59B7 reg_map
      >>= fun reg_map ->
      reg_name_to_dword_reg "ecx" >>= fun ecx -> reg_val_get ecx reg_map)
      (return 0xABCD59B7)
  ;;

  let%test _ =
    do_test
      (reg_name_to_dword_reg "edx"
      >>= fun edx ->
      reg_map
      >>= fun reg_map ->
      reg_val_set edx 0x7654321A reg_map
      >>= fun reg_map ->
      reg_name_to_dword_reg "edx" >>= fun edx -> reg_val_get edx reg_map)
      (return 0x7654321A)
  ;;

  let%test _ =
    do_test
      (reg_name_to_byte_reg "ah"
      >>= fun ah ->
      reg_map
      >>= fun reg_map ->
      reg_val_set ah 0xABC reg_map
      >>= fun reg_map ->
      reg_name_to_dword_reg "eax" >>= fun eax -> reg_val_get eax reg_map)
      (return 0xABCDBC34)
  ;;

  let%test _ =
    do_test
      (reg_name_to_word_reg "ax"
      >>= fun ax ->
      reg_map
      >>= fun reg_map ->
      reg_val_set ax 0xABCDEF reg_map
      >>= fun reg_map ->
      reg_name_to_dword_reg "eax" >>= fun eax -> reg_val_get eax reg_map)
      (return 0xABCDCDEF)
  ;;
end
