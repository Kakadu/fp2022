(** Copyright 2021-2022, andreyizrailev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base

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

  (* Get register id *)
  val reg_id_to_int : 'a reg -> int

  (* Get value of a constant *)
  val const_val : 'a const -> int
  val reg_name_to_int : string -> int
  val reg_name_to_byte_reg : string -> byte reg
  val reg_name_to_word_reg : string -> word reg
  val reg_name_to_dword_reg : string -> dword reg
end = struct
  type byte
  type word
  type dword

  let pp_byte fmt _ = Format.fprintf fmt "byte"
  let pp_word fmt _ = Format.fprintf fmt "word"
  let pp_dword fmt _ = Format.fprintf fmt "dword"

  type 'a reg = int [@@deriving show { with_path = false }]
  type 'a const = int [@@deriving show { with_path = false }]

  let int_to_byte_const x =
    if x < -(2 ** 7) || x > (2 ** 7) - 1 then failwith "Int8 expected" else x
  ;;

  let int_to_word_const x =
    if x < -(2 ** 15) || x > (2 ** 15) - 1 then failwith "Int16 expected" else x
  ;;

  let int_to_dword_const = Fun.id
  let int_to_byte_reg = Fun.id
  let int_to_word_reg = Fun.id
  let int_to_dword_reg = Fun.id
  let byte_reg_name_list = [ "ah"; "al"; "bh"; "bl"; "ch"; "cl"; "dh"; "dl" ]
  let word_reg_name_list = [ "ax"; "bx"; "cx"; "dx" ]
  let dword_reg_name_list = [ "eax"; "ebx"; "ecx"; "edx" ]
  let all_reg_name_list = byte_reg_name_list @ word_reg_name_list @ dword_reg_name_list
  let reg_id_to_int = Fun.id
  let const_val = Fun.id

  let reg_name_to_int reg_name =
    (* Find index of element [reg_name] *)
    let rec helper (idx : int) = function
      | [] -> failwith ("No register called \"" ^ reg_name ^ "\"")
      | h :: tl -> if String.compare h reg_name = 0 then idx else helper (idx + 1) tl
    in
    helper 0 all_reg_name_list
  ;;

  let reg_name_to_byte_reg reg_name = reg_name |> reg_name_to_int |> int_to_byte_reg
  let reg_name_to_word_reg reg_name = reg_name |> reg_name_to_int |> int_to_word_reg
  let reg_name_to_dword_reg reg_name = reg_name |> reg_name_to_int |> int_to_dword_reg
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

module CmdHandler = struct
  let cmd_one_arg_list = [ "inc"; "mul" ]
  let cmd_two_args_list = [ "mov"; "add"; "sub" ]

  let cmd_one_arg_str_to_alg = function
    | "inc" -> fun x -> Inc x
    | "mul" -> fun x -> Mul x
    | str -> failwith ("Unknown command " ^ str)
  ;;

  let cmd_two_args_str_to_alg = function
    | "mov" -> fun x -> Mov x
    | "add" -> fun x -> Add x
    | "sub" -> fun x -> Sub x
    | str -> failwith ("Unknown command " ^ str)
  ;;
end
