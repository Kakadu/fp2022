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

let space_p = satisfy is_space <?> "space_p"

(* Get rid of some number of zero or more spaces *)
let spaces_p = skip_many space_p <?> "spaces_p"

(* Get rid of some number of one or more spaces *)
let spaces1_p = skip_many1 space_p <?> "spaces1_p"
let eol_p = char '\n' <?> "eol_p"
let trim_p p = spaces_p *> p <* spaces_p
let comma_p = trim_p (char ',') <?> "comma_p"
let skip_empty_lines_p = skip_many (space_p <|> eol_p) <?> "skip_empty_lines_p"

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_label_char = function
  | '_' | '$' | '#' | '@' | '~' | '.' | '?' -> true
  | c -> is_digit c || is_letter c
;;

(* Parse label declaration (e.g. "l1:") *)
let label_decl_p =
  (* The label must start with a letter *)
  lift2 (fun c s -> Char.escaped c ^ s) (satisfy is_letter) (take_while is_label_char)
  <* char ':'
  >>| (fun s -> LCommand s)
  <?> "label_decl_p"
;;

(* Parse an integer and return it as integer *)
let int_p =
  let sign_p = string "+" <|> string "-" in
  let const_str_p = lift2 ( ^ ) (option "" sign_p) (take_while1 is_digit) in
  const_str_p >>| int_of_string <?> "int_p"
;;

(* const_str_p *)
(* >>| fun x -> *)
(* let y = int_of_string x in *)
(* print_endline "IN INT_P"; *)
(* print_int y; *)
(* print_endline (string_of_int y); *)
(* y *)

(* Generate parser for an integer *)
let gen_const_p int_is_t_const int_to_t_const =
  int_p
  >>= fun x ->
  (* print_endline "AFTER INT_P"; *)
  if int_is_t_const x
  then (* print_endline "IN THEN"; *)
    return (Const (int_to_t_const x))
  else (* print_endline "IN ELSE"; *)
    fail "Integer is too big"
;;

(* then return (Const (int_to_t_const x)) *)
(* else fail "Integer is too big" *)

(****************************************************************************************)
(* Parse an integer and return it as a Const (...) *)
let bconst_p = gen_const_p int_is_byte_const int_to_byte_const <?> "bconst_p"
let wconst_p = gen_const_p int_is_word_const int_to_word_const <?> "wconst_p"
let dconst_p = gen_const_p int_is_dword_const int_to_dword_const <?> "dconst_p"

(****************************************************************************************)
(* Generate parser for register names that returns a string *)
let gen_reg_name_p reg_name_list = choice (List.map string reg_name_list)

(* Parse register name and return it as a string *)
let breg_name_p = gen_reg_name_p byte_reg_name_list <?> "breg_name_p"
let wreg_name_p = gen_reg_name_p word_reg_name_list <?> "wreg_name_p"
let dreg_name_p = gen_reg_name_p dword_reg_name_list <?> "dreg_name_p"

(****************************************************************************************)
(* Generate parser for register names that returns Reg (...) *)
let gen_reg_p reg_name_p reg_name_to_t_reg =
  reg_name_p >>| fun reg_name -> Reg (reg_name_to_t_reg reg_name)
;;

(* Parse register name and convert them to Reg (...) *)
let breg_p = gen_reg_p breg_name_p reg_name_to_byte_reg <?> "breg_p"
let wreg_p = gen_reg_p wreg_name_p reg_name_to_word_reg <?> "wreg_p"
let dreg_p = gen_reg_p dreg_name_p reg_name_to_dword_reg <?> "dreg_p"

(****************************************************************************************)
(* Generate parser for two registers that returns RegReg (...) *)
let gen_regreg_p reg_name_p reg_name_to_t_reg =
  lift2
    (fun reg_name1 reg_name2 ->
      RegReg (reg_name_to_t_reg reg_name1, reg_name_to_t_reg reg_name2))
    (reg_name_p <* comma_p)
    reg_name_p
;;

(* Parser two registers and convert them to RegReg (...) *)
let bregreg_p = gen_regreg_p breg_name_p reg_name_to_byte_reg <?> "breg_const_p"
let wregreg_p = gen_regreg_p wreg_name_p reg_name_to_word_reg <?> "wregreg_p"
let dregreg_p = gen_regreg_p dreg_name_p reg_name_to_dword_reg <?> "dregreg_p"

(****************************************************************************************)
(* Generate parser for register and constant that returns RegConst (...) *)
let gen_regconst_p reg_name_p reg_name_to_t_reg int_to_t_const =
  lift2
    (fun reg_name integer ->
      RegConst (reg_name_to_t_reg reg_name, int_to_t_const integer))
    (reg_name_p <* comma_p)
    int_p
;;

(* Parse register and constant and convert them to RegConst (...) *)
let bregconst_p =
  gen_regconst_p breg_name_p reg_name_to_byte_reg int_to_byte_const <?> "bregconst_p"
;;

let wregconst_p =
  gen_regconst_p wreg_name_p reg_name_to_word_reg int_to_word_const <?> "wregconst_p"
;;

let dregconst_p =
  gen_regconst_p dreg_name_p reg_name_to_dword_reg int_to_dword_const <?> "dregconst_p"
;;

(****************************************************************************************)
(* Generate a parser of one-line command *)
let gen_command_p operand_p converter cmd_str =
  string cmd_str *> spaces1_p *> operand_p >>| converter
;;

(****************************************************************************************)
(* Generate a parser of a byte command *)
let gen_bcommand_p operand_p cmd_str_to_command cmd_str =
  gen_command_p operand_p (fun x -> BCommand (cmd_str_to_command cmd_str x)) cmd_str
;;

(* Generate a parser of one-arg byte command *)
let gen_bcommand_one_arg_p =
  gen_bcommand_p (breg_p <|> bconst_p) cmd_one_arg_str_to_command
;;

(* Generate a parser of two-args byte command *)
let gen_bcommand_two_args_p =
  gen_bcommand_p (bregreg_p <|> bregconst_p) cmd_two_args_str_to_command
;;

(****************************************************************************************)
(* Generate a parser of a word command *)
let gen_wcommand_p operand_p cmd_str_to_command cmd_str =
  gen_command_p operand_p (fun x -> WCommand (cmd_str_to_command cmd_str x)) cmd_str
;;

(* Generate a parser of one-arg word command *)
let gen_wcommand_one_arg_p =
  gen_wcommand_p (wreg_p <|> wconst_p) cmd_one_arg_str_to_command
;;

(* Generate a parser of two-args word command *)
let gen_wcommand_two_args_p =
  gen_wcommand_p (wregreg_p <|> wregconst_p) cmd_two_args_str_to_command
;;

(****************************************************************************************)
(* Generate a parser of a dword command *)
let gen_dcommand_p operand_p cmd_str_to_command cmd_str =
  gen_command_p operand_p (fun x -> DCommand (cmd_str_to_command cmd_str x)) cmd_str
;;

(* Generate a parser of one-arg dword command *)
let gen_dcommand_one_arg_p =
  gen_dcommand_p (dreg_p <|> dconst_p) cmd_one_arg_str_to_command
;;

(* Generate a parser of two-args word command *)
let gen_dcommand_two_args_p =
  gen_dcommand_p (dregreg_p <|> dregconst_p) cmd_two_args_str_to_command
;;

(****************************************************************************************)
(* The following three parsers are intended to parse a one-line command *)
let bcommand_p =
  choice
    (List.map gen_bcommand_one_arg_p cmd_one_arg_list
    @ List.map gen_bcommand_two_args_p cmd_two_args_list)
  <?> "bcommand_p"
;;

let wcommand_p =
  choice
    (List.map gen_wcommand_one_arg_p cmd_one_arg_list
    @ List.map gen_wcommand_two_args_p cmd_two_args_list)
  <?> "wcommand_p"
;;

let dcommand_p =
  choice
    (List.map gen_dcommand_one_arg_p cmd_one_arg_list
    @ List.map gen_dcommand_two_args_p cmd_two_args_list)
  <?> "dcommand_p"
;;

(****************************************************************************************)
(* Parse any instruction == line *)
let instr_p = choice [ dcommand_p; wcommand_p; bcommand_p; label_decl_p ] |> trim_p

(* Parse the whole NASM program *)
let program_p =
  skip_empty_lines_p *> sep_by1 (eol_p *> skip_empty_lines_p) instr_p
  <* skip_empty_lines_p
;;

(****************************************************************************************)
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

let ok_instruction = test_ok pp_instruction
let fail_instruction = test_fail pp_instruction

let%test _ =
  ok_instruction bcommand_p "inc ah" (BCommand (Inc (Reg (reg_name_to_byte_reg "ah"))))
;;

let%test _ =
  ok_instruction wcommand_p "mul bx" (WCommand (Mul (Reg (reg_name_to_word_reg "bx"))))
;;

let%test _ = fail_instruction dcommand_p "inc dl"

let%test _ =
  ok_instruction
    dcommand_p
    "mov ecx, edx"
    (DCommand (Mov (RegReg (reg_name_to_dword_reg "ecx", reg_name_to_dword_reg "edx"))))
;;

let%test _ =
  ok_instruction
    wcommand_p
    "add bx, 1578"
    (WCommand (Add (RegConst (reg_name_to_word_reg "bx", int_to_word_const 1578))))
;;

let%test _ = fail_instruction bcommand_p "inc 314513245"
(* let%test _ = fail_instruction bcommand_p "sub al, -1234" *)
let%test _ = fail_instruction instr_p "add al, edx"
let%test _ = ok_instruction instr_p "abc?def$:" (LCommand "abc?def$")
let%test _ = fail_instruction instr_p "@abc:"

let ok_all_instructions = test_ok pp_all_instructions
let fail_all_instructions = test_fail pp_all_instructions

let%test _ =
  ok_all_instructions
    program_p
    "mov ax, bx"
    [ WCommand (Mov (RegReg (reg_name_to_word_reg "ax", reg_name_to_word_reg "bx"))) ]
;;

let%test _ =
  ok_all_instructions
    program_p
    "mov ax, bx\n     add eax, ecx"
    [ WCommand (Mov (RegReg (reg_name_to_word_reg "ax", reg_name_to_word_reg "bx")))
    ; DCommand (Add (RegReg (reg_name_to_dword_reg "eax", reg_name_to_dword_reg "ecx")))
    ]
;;

let%test _ =
  ok_all_instructions
    program_p
    "mov ax, bx\n     add eax, ecx\n inc ax  "
    [ WCommand (Mov (RegReg (reg_name_to_word_reg "ax", reg_name_to_word_reg "bx")))
    ; DCommand (Add (RegReg (reg_name_to_dword_reg "eax", reg_name_to_dword_reg "ecx")))
    ; WCommand (Inc (Reg (reg_name_to_word_reg "ax")))
    ]
;;

let%test _ =
  ok_all_instructions
    program_p
    "l1:\n mov ax, bx\n add eax, ecx\n inc bl\n l@abel2:   \n sub dh, 5\n\n\n\n   \n"
    [ LCommand "l1"
    ; WCommand (Mov (RegReg (reg_name_to_word_reg "ax", reg_name_to_word_reg "bx")))
    ; DCommand (Add (RegReg (reg_name_to_dword_reg "eax", reg_name_to_dword_reg "ecx")))
    ; BCommand (Inc (Reg (reg_name_to_byte_reg "bl")))
    ; LCommand "l@abel2"
    ; BCommand (Sub (RegConst (reg_name_to_byte_reg "dh", int_to_byte_const 5)))
    ]
;;

let%test _ = fail_all_instructions program_p "mov ax, bx   inc ax"
let%test _ = fail_all_instructions program_p "label_without_colon"
