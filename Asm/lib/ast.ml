(** Copyright 2021-2022, andreyizrailev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
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
  | Movdqa of xmm operand_single
  | Addpd of xmm operands_double
  | Mulpd of xmm operands_double
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
  (* Command with xmm registers *)
  | XCommand of xmm command
  (* Command with a label/string operand.
     Our type system prevents us from having
     SCommand (Inc (...)) or SCommand (Je (Reg (...))) *)
  | SCommand of string command
[@@deriving show { with_path = false }]

type ast = instruction list [@@deriving show { with_path = false }]

module CmdHandler = struct
  let lcommand v = LCommand v
  let bcommand v = BCommand v
  let wcommand v = WCommand v
  let dcommand v = DCommand v
  let xcommand v = XCommand v
  let scommand v = SCommand v
  let cmd_zero_args_list = [ "ret" ]
  let cmd_one_arg_list = [ "inc"; "mul"; "push"; "pop" ]
  let cmd_two_args_list = [ "mov"; "add"; "sub"; "cmp" ]
  let xcmd_one_arg_list = [ "movdqa" ]
  let xcmd_two_args_list = [ "addpd"; "mulpd" ]
  let scmd_list = [ "jmp"; "je"; "jne"; "call" ]

  let cmd_zero_args_str_to_command = function
    | "ret" -> Ret
    | str -> failwith ("Unknown command " ^ str)
  ;;

  let cmd_one_arg_str_to_command cmd x =
    match cmd with
    | "inc" -> Inc x
    | "mul" -> Mul x
    | "push" -> Push x
    | "pop" -> Pop x
    | str -> failwith ("Unknown command " ^ str)
  ;;

  let cmd_two_args_str_to_command cmd x =
    match cmd with
    | "mov" -> Mov x
    | "add" -> Add x
    | "sub" -> Sub x
    | "cmp" -> Cmp x
    | str -> failwith ("Unknown command " ^ str)
  ;;

  let xcmd_one_arg_str_to_command cmd x =
    match cmd with
    | "movdqa" -> Movdqa x
    | str -> failwith ("Unknown command " ^ str)
  ;;

  let xcmd_two_args_str_to_command cmd x =
    match cmd with
    | "addpd" -> Addpd x
    | "mulpd" -> Mulpd x
    | str -> failwith ("Unknown command " ^ str)
  ;;

  (* This is a special case for commands that take a string rather than Reg or
     Const. Such commands always have only one operand *)
  let scmd_str_to_command cmd x =
    match cmd with
    | "jmp" -> Jmp x
    | "je" -> Je x
    | "jne" -> Jne x
    | "call" -> Call x
    | str -> failwith ("Unknown command " ^ str)
  ;;
end
