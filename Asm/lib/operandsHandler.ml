(** Copyright 2021-2022, andreyizrailev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Utils

type byte
type word
type dword

let pp_byte fmt _ = Format.fprintf fmt "byte"
let pp_word fmt _ = Format.fprintf fmt "word"
let pp_dword fmt _ = Format.fprintf fmt "dword"

type 'a reg = int [@@deriving show { with_path = false }]
type 'a const = int [@@deriving show { with_path = false }]

let int_is_byte_const x = x >= -(2 ** 7) && x <= (2 ** 7) - 1
let int_is_word_const x = x >= -(2 ** 15) && x <= (2 ** 15) - 1
let int_is_dword_const x = x >= -(2 ** 31) && x <= (2 ** 31) - 1

(* Though the following three function may look strange, they copy NASM's behaviour *)
let int_to_byte_const x =
  if not (int_is_byte_const x) then failwith "Int8 expected" else if x < 0 then 0 else x
;;

let int_to_word_const x =
  if not (int_is_word_const x) then failwith "Int16 expected" else if x < 0 then 0 else x
;;

let int_to_dword_const x =
  if not (int_is_dword_const x) then failwith "Int32 expected" else if x < 0 then 0 else x
;;

(* Lists of register names *)
let byte_reg_name_list = [ "ah"; "al"; "bh"; "bl"; "ch"; "cl"; "dh"; "dl" ]
let word_reg_name_list = [ "ax"; "bx"; "cx"; "dx" ]
let dword_reg_name_list = [ "eax"; "ebx"; "ecx"; "edx" ]

(* Lengths of each of the lists just to avoid calculating them everytime we need them *)
let byte_reg_list_len = List.length byte_reg_name_list
let word_reg_list_len = List.length word_reg_name_list
let dword_reg_list_len = List.length dword_reg_name_list

(* Functions to check if the id belongs to a register of particular size *)
let reg_id_is_byte_reg reg_id = reg_id < byte_reg_list_len
let reg_id_is_dword_reg reg_id = reg_id >= byte_reg_list_len + word_reg_list_len

let reg_id_is_word_reg reg_id =
  (not (reg_id_is_byte_reg reg_id)) && not (reg_id_is_dword_reg reg_id)
;;

(* Convert register id to 'a reg *)
let int_to_byte_reg reg_id =
  if reg_id_is_byte_reg reg_id
  then reg_id
  else failwith (Printf.sprintf "Register with id %d is not a byte register" reg_id)
;;

let int_to_word_reg reg_id =
  if reg_id_is_word_reg reg_id
  then reg_id
  else failwith (Printf.sprintf "Register with id %d is not a word register" reg_id)
;;

let int_to_dword_reg reg_id =
  if reg_id_is_dword_reg reg_id
  then reg_id
  else failwith (Printf.sprintf "Register with id %d is not a dword register" reg_id)
;;

let all_reg_name_list = byte_reg_name_list @ word_reg_name_list @ dword_reg_name_list
let reg_to_id : 'a reg -> int = Fun.id
let const_val : 'a const -> int = Fun.id

let reg_name_to_id reg_name =
  match List.index_of_elem reg_name String.equal all_reg_name_list with
  | None -> failwith (Printf.sprintf "No register called %S" reg_name)
  | Some x -> x
;;

let reg_name_to_byte_reg reg_name = reg_name |> reg_name_to_id |> int_to_byte_reg
let reg_name_to_word_reg reg_name = reg_name |> reg_name_to_id |> int_to_word_reg
let reg_name_to_dword_reg reg_name = reg_name |> reg_name_to_id |> int_to_dword_reg

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

let reg_val_get reg reg_map =
  let reg_id = reg_to_id reg in
  let primary_id = reg_get_primary_id reg_id in
  let primary_val = IntMap.find primary_id reg_map in
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
  IntMap.add primary_id value_to_set reg_map
;;

module RegMapTest = struct
  open IntMap

  let%test _ =
    reg_get_primary_id (reg_name_to_dword_reg "ecx") = reg_name_to_dword_reg "ecx"
  ;;

  let%test _ =
    reg_get_primary_id (reg_name_to_word_reg "bx") = reg_name_to_dword_reg "ebx"
  ;;

  let%test _ =
    reg_get_primary_id (reg_name_to_byte_reg "dh") = reg_name_to_dword_reg "edx"
  ;;

  let%test _ =
    reg_get_primary_id (reg_name_to_byte_reg "al") = reg_name_to_dword_reg "eax"
  ;;

  let reg_map =
    empty
    |> add (reg_name_to_dword_reg "eax") 0xABCD1234
    |> add (reg_name_to_dword_reg "ebx") 0
    |> add (reg_name_to_dword_reg "ecx") 0
    |> add (reg_name_to_dword_reg "edx") 0
  ;;

  let%test _ = reg_val_get (reg_name_to_dword_reg "eax") reg_map = 0xABCD1234
  let%test _ = reg_val_get (reg_name_to_word_reg "ax") reg_map = 0x1234
  let%test _ = reg_val_get (reg_name_to_byte_reg "al") reg_map = 0x34
  let%test _ = reg_val_get (reg_name_to_byte_reg "ah") reg_map = 0x12

  let reg_map = reg_map |> add (reg_name_to_dword_reg "ecx") 0xABCD1234

  let%test _ =
    reg_val_set (reg_name_to_byte_reg "ch") 0x10 reg_map
    |> reg_val_get (reg_name_to_dword_reg "ecx")
    = 0xABCD1034
  ;;

  let%test _ =
    reg_val_set (reg_name_to_byte_reg "cl") 0x0 reg_map
    |> reg_val_get (reg_name_to_dword_reg "ecx")
    = 0xABCD1200
  ;;

  let%test _ =
    reg_val_set (reg_name_to_word_reg "cx") 0x59B7 reg_map
    |> reg_val_get (reg_name_to_dword_reg "ecx")
    = 0xABCD59B7
  ;;

  let%test _ =
    reg_val_set (reg_name_to_dword_reg "edx") 0x7654321A reg_map
    |> reg_val_get (reg_name_to_dword_reg "edx")
    = 0x7654321A
  ;;

  let%test _ =
    reg_val_set (reg_name_to_byte_reg "ah") 0xABC reg_map
    |> reg_val_get (reg_name_to_dword_reg "eax")
    = 0xABCDBC34
  ;;

  let%test _ =
    reg_val_set (reg_name_to_word_reg "ax") 0xABCDEF reg_map
    |> reg_val_get (reg_name_to_dword_reg "eax")
    = 0xABCDCDEF
  ;;
end
