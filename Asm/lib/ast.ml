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
  let int_to_byte_const x = if int_is_byte_const x then x else failwith "Int8 expected"
  let int_to_word_const x = if int_is_word_const x then x else failwith "Int16 expected"
  let int_to_dword_const x = if int_is_dword_const x then x else failwith "Int32 expected"
  let int_to_byte_reg = Fun.id
  let int_to_word_reg = Fun.id
  let int_to_dword_reg = Fun.id
  let byte_reg_name_list = [ "ah"; "al"; "bh"; "bl"; "ch"; "cl"; "dh"; "dl" ]
  let word_reg_name_list = [ "ax"; "bx"; "cx"; "dx" ]
  let dword_reg_name_list = [ "eax"; "ebx"; "ecx"; "edx" ]
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
type all_instructions = instruction list [@@deriving show { with_path = false }]

module CmdHandler = struct
  let cmd_one_arg_list = [ "inc"; "mul"; "push"; "pop" ]
  let cmd_two_args_list = [ "mov"; "add"; "sub" ]
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
