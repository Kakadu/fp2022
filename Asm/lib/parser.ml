(** Copyright 2021-2022, andreyizrailev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Ast.OperandsHandler
open Ast.CmdHandler

let is_space = function
  | ' ' | '\t' -> true
  | _ -> false
;;

let spaces_p = skip_while is_space
let eol_p = char '\n'
let trim_p p = spaces_p *> p <* spaces_p

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

(* Parse an integer. Ast will handle if its size is ok *)
let const_p =
  let sign_p = string "+" <|> string "-" in
  let const_str_p = lift2 ( ^ ) (option "" sign_p) (take_while1 is_digit) in
  const_str_p >>| int_of_string
;;

let bconst_p = const_p >>| fun x -> Const (int_to_byte_const x)
let wconst_p = const_p >>| fun x -> Const (int_to_word_const x)
let dconst_p = const_p >>| fun x -> Const (int_to_dword_const x)

(* Parse register names *)
let breg_name_p = choice (List.map string byte_reg_name_list)
let wreg_name_p = choice (List.map string word_reg_name_list)
let dreg_name_p = choice (List.map string dword_reg_name_list)

(* Parse register names and convert them to operand_single *)
let breg_p = breg_name_p >>| fun reg_name -> Reg (reg_name_to_byte_reg reg_name)
let wreg_p = wreg_name_p >>| fun reg_name -> Reg (reg_name_to_word_reg reg_name)
let dreg_p = dreg_name_p >>| fun reg_name -> Reg (reg_name_to_dword_reg reg_name)

let bregreg_p =
  lift2
    (fun reg1_name reg2_name ->
      RegReg (reg_name_to_byte_reg reg1_name, reg_name_to_byte_reg reg2_name))
    breg_name_p
    breg_name_p
;;

let gen_bcommand_one_arg_parser cmd =
  trim_p (string cmd) *> (breg_p <|> bconst_p)
  >>| fun x -> BCommand (cmd_one_arg_str_to_alg cmd x)
;;

let gen_wcommand_one_arg_parser cmd =
  trim_p (string cmd) *> (wreg_p <|> wconst_p)
  >>| fun x -> WCommand (cmd_one_arg_str_to_alg cmd x)
;;

let gen_dcommand_one_arg_parser cmd =
  trim_p (string cmd) *> (dreg_p <|> dconst_p)
  >>| fun x -> DCommand (cmd_one_arg_str_to_alg cmd x)
;;

let bcommand_p =
  choice
    (List.map
       gen_bcommand_one_arg_parser
       cmd_one_arg_list (* @ List.map gen_bcommand_two_args_parser cmd_two_args_list *))
;;

let wcommand_p =
  choice
    (List.map
       gen_wcommand_one_arg_parser
       cmd_one_arg_list (* @ List.map gen_bcommand_two_args_parser cmd_two_args_list *))
;;

let dcommand_p =
  choice
    (List.map
       gen_dcommand_one_arg_parser
       cmd_one_arg_list (* @ List.map gen_bcommand_two_args_parser cmd_two_args_list *))
;;

(* let bcommand_p = *)
(*   trim_p (string "inc") *> breg_p *)
(*   >>| (fun x -> BCommand (Inc x)) *)
(*   <|> (trim_p (string "mov") *> bregreg_p >>| fun x -> BCommand (Mov x)) *)
(*   <|> (trim_p (string "add") *> bregreg_p >>| fun x -> BCommand (Add x)) *)
(* ;; *)

(* Taken from vs9h *)
let test_ok, test_fail =
  let ok ppf parser input expected =
    match parse_string ~consume:All parser input with
    | Ok res when expected = res -> true
    | Ok res ->
      ppf Format.std_formatter res;
      false
    | Error e ->
      print_string ("Failed to parse \"" ^ input ^ "\"" ^ e);
      false
  in
  let fail ppf parser input =
    match parse_string ~consume:All parser input with
    | Ok res ->
      ppf Format.std_formatter res;
      false
    | _ -> true
  in
  ok, fail
;;

let ok_string = test_ok (fun _ -> print_string)
let fail_string = test_fail (fun _ -> print_string)
let ok_int = test_ok (fun _ -> print_int)
let fail_int = test_fail (fun _ -> print_int)

let%test _ = ok_string (trim_p (string "test")) "    test " "test"
let%test _ = ok_string breg_name_p "ah" "ah"
let%test _ = fail_string breg_name_p "ax"
