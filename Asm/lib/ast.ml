(** Copyright 2021-2022, andreyizrailev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base

module OperandsHandler : sig
  (* phantom types *)
  type byte
  type word
  type dword

  val pp_byte : Format.formatter -> byte -> unit
  val pp_word : Format.formatter -> word -> unit
  val pp_dword : Format.formatter -> dword -> unit

  type 'a reg [@@deriving show { with_path = false }]
  type 'a const [@@deriving show { with_path = false }]

  val to_int8 : int -> byte const
  val to_int16 : int -> word const
  val to_int32 : int -> dword const
  val to_byte_reg : int -> byte reg
  val to_word_reg : int -> word reg
  val to_dword_reg : int -> dword reg

  (* get register id *)
  val reg_id : 'a reg -> int

  (* get value of a constant *)
  val const_val : 'a const -> int
end = struct
  type byte
  type word
  type dword

  let pp_byte fmt _ = Format.fprintf fmt "byte"
  let pp_word fmt _ = Format.fprintf fmt "word"
  let pp_dword fmt _ = Format.fprintf fmt "dword"

  type 'a reg = int [@@deriving show { with_path = false }]
  type 'a const = int [@@deriving show { with_path = false }]

  let to_int8 x =
    if x < -(2 ** 7) || x > (2 ** 7) - 1 then failwith "Int8 expected" else x
  ;;

  let to_int16 x =
    if x < -(2 ** 15) || x > (2 ** 15) - 1 then failwith "Int16 expected" else x
  ;;

  let to_int32 = Fun.id
  let to_byte_reg = Fun.id
  let to_word_reg = Fun.id
  let to_dword_reg = Fun.id
  let reg_id = Fun.id
  let const_val = Fun.id
end

open OperandsHandler

type 'a operands_double =
  | RegReg of 'a reg * 'a reg
  | RegConst of 'a reg * 'a const
[@@deriving show { with_path = false }]

type 'a operand_single =
  | Reg of 'a reg
  | Const of 'a const
[@@deriving show { with_path = false }]

type 'a command =
  | Mov of 'a operands_double
  | Add of 'a operands_double
  | Sub of 'a operands_double
  | Inc of 'a operand_single
  | Mul of 'a operand_single
  | Ret
[@@deriving show { with_path = false }]

type instruction =
  | Label of string
  | BCommand of byte command
  | WCommand of word command
  | DCommand of dword command
[@@deriving show { with_path = false }]

type all_instructions = instruction list [@@deriving show { with_path = false }]
