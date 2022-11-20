(** Copyright 2021-2022, andreyizrailev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Utils

module OperandsHandler : sig
  (* Phantom types *)
  type byte
  type word
  type dword

  (* Needed for pretty printing *)
  val pp_byte : Format.formatter -> byte -> unit
  val pp_word : Format.formatter -> word -> unit
  val pp_dword : Format.formatter -> dword -> unit

  (* Operand types *)
  type 'a reg [@@deriving show { with_path = false }]
  type 'a const [@@deriving show { with_path = false }]

  (* Check integer size *)
  val int_is_byte_const : int -> bool
  val int_is_word_const : int -> bool
  val int_is_dword_const : int -> bool

  (* Converters *)
  val int_to_byte_const : int -> byte const
  val int_to_word_const : int -> word const
  val int_to_dword_const : int -> dword const
  val int_to_byte_reg : int -> byte reg
  val int_to_word_reg : int -> word reg
  val int_to_dword_reg : int -> dword reg
  val byte_reg_name_list : string list
  val word_reg_name_list : string list
  val dword_reg_name_list : string list
  val reg_name_to_byte_reg : string -> byte reg
  val reg_name_to_word_reg : string -> word reg
  val reg_name_to_dword_reg : string -> dword reg

  (* Get internal register id by its name *)
  val reg_name_to_id : string -> int

  (* Get integer value of a constant *)
  val const_val : 'a const -> int

  (* These two functions require us to have ids of all dword registers as keys
     in the reg_map *)
  val reg_val_get : 'a reg -> int IntMap.t -> int

  (* If the value to set is too big, only the last bytes are considered.
     E.g. if we try to set "ah" to 0xABC, it will be set to 0xBC *)
  val reg_val_set : 'a reg -> int -> int IntMap.t -> int IntMap.t
end = struct
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
    if not (int_is_word_const x)
    then failwith "Int16 expected"
    else if x < 0
    then 0
    else x
  ;;

  let int_to_dword_const x =
    if not (int_is_dword_const x)
    then failwith "Int32 expected"
    else if x < 0
    then 0
    else x
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
    else failwith ("Register with id " ^ string_of_int reg_id ^ " is not a byte register")
  ;;

  let int_to_word_reg reg_id =
    if reg_id_is_word_reg reg_id
    then reg_id
    else failwith ("Register with id " ^ string_of_int reg_id ^ " is not a word register")
  ;;

  let int_to_dword_reg reg_id =
    if reg_id_is_dword_reg reg_id
    then reg_id
    else failwith ("Register with id " ^ string_of_int reg_id ^ " is not a dword register")
  ;;

  let all_reg_name_list = byte_reg_name_list @ word_reg_name_list @ dword_reg_name_list
  let reg_to_id : 'a reg -> int = Fun.id
  let const_val : 'a const -> int = Fun.id

  let reg_name_to_id reg_name =
    match List.index_of_elem reg_name String.equal all_reg_name_list with
    | None -> failwith ("No register called \"" ^ reg_name ^ "\"")
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
end

open OperandsHandler

type 'a operands_double =
  | RegReg of 'a reg * 'a reg
  | RegConst of 'a reg * 'a const
[@@deriving show { with_path = false }]

type 'a operand_single =
  | Reg of 'a reg
  | Const of 'a const
  | Label of string
[@@deriving show { with_path = false }]

type 'a command =
  | Mov of 'a operands_double
  | Add of 'a operands_double
  | Sub of 'a operands_double
  | Cmp of 'a operands_double
  | Inc of 'a operand_single
  | Mul of 'a operand_single
  | Push of 'a operand_single
  | Pop of 'a operand_single
  | Jmp of string operand_single
  | Je of string operand_single
  | Jne of string operand_single
  | Call of string operand_single
  | Ret
[@@deriving show { with_path = false }]

type instruction =
  (* Label declaration *)
  | LCommand of string
  (* Command with byte-size operands *)
  | BCommand of byte command
  (* Command with word-size operands *)
  | WCommand of word command
  (* Command with dword-size operands *)
  | DCommand of dword command
  (* Command with a label/string operand.
     Our type system prevents us from having
     SCommand (Inc (...)) or SCommand (Je (Reg (...))) *)
  | SCommand of string command
[@@deriving show { with_path = false }]

(* For now the AST may contain invalid instructions like Inc (Const 5).
   It should be fixed, probably by scanning the AST after parsing and
   producing an error if an invalid instruction is found *)
type ast = instruction list [@@deriving show { with_path = false }]

module CmdHandler = struct
  let cmd_one_arg_list = [ "inc"; "mul"; "push"; "pop" ]
  let cmd_two_args_list = [ "mov"; "add"; "sub"; "cmp" ]
  let scmd_list = [ "jmp"; "je"; "jne"; "call" ]

  let cmd_one_arg_str_to_command = function
    | "inc" -> fun x -> Inc x
    | "mul" -> fun x -> Mul x
    | "push" -> fun x -> Push x
    | "pop" -> fun x -> Pop x
    | str -> failwith ("Unknown command " ^ str)
  ;;

  let cmd_two_args_str_to_command = function
    | "mov" -> fun x -> Mov x
    | "add" -> fun x -> Add x
    | "sub" -> fun x -> Sub x
    | "cmp" -> fun x -> Cmp x
    | str -> failwith ("Unknown command " ^ str)
  ;;

  (* This is a special case for commands that take a string rather than Reg or
     Const. Such commands always have only one operand *)
  let scmd_str_to_command = function
    | "jmp" -> fun x -> Jmp x
    | "je" -> fun x -> Je x
    | "jne" -> fun x -> Jne x
    | "call" -> fun x -> Call x
    | str -> failwith ("Unknown command " ^ str)
  ;;
end
