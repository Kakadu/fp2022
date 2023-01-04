(** Copyright 2021-2023, andreyizrailev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open MonadError

module Ast (M : MonadError) = struct
  open OperandsHandler.OperandsHandler (M)

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
end

module CmdHandler (M : MonadError) = struct
  open Ast (M)
  open M

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
    | "ret" -> return Ret
    | str -> error ("Unknown command " ^ str)
  ;;

  let cmd_one_arg_str_to_command cmd x =
    match cmd with
    | "inc" -> return (Inc x)
    | "mul" -> return (Mul x)
    | "push" -> return (Push x)
    | "pop" -> return (Pop x)
    | str -> error ("Unknown command " ^ str)
  ;;

  let cmd_two_args_str_to_command cmd x =
    match cmd with
    | "mov" -> return (Mov x)
    | "add" -> return (Add x)
    | "sub" -> return (Sub x)
    | "cmp" -> return (Cmp x)
    | str -> error ("Unknown command " ^ str)
  ;;

  let xcmd_one_arg_str_to_command cmd x =
    match cmd with
    | "movdqa" -> return (Movdqa x)
    | str -> error ("Unknown command " ^ str)
  ;;

  let xcmd_two_args_str_to_command cmd x =
    match cmd with
    | "addpd" -> return (Addpd x)
    | "mulpd" -> return (Mulpd x)
    | str -> error ("Unknown command " ^ str)
  ;;

  (* This is a special case for commands that take a string rather than Reg or
     Const. Such commands always have only one operand *)
  let scmd_str_to_command cmd x =
    match cmd with
    | "jmp" -> return (Jmp x)
    | "je" -> return (Je x)
    | "jne" -> return (Jne x)
    | "call" -> return (Call x)
    | str -> error ("Unknown command " ^ str)
  ;;
end
