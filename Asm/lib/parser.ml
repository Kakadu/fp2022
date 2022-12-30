(** Copyright 2021-2022, andreyizrailev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open OperandsHandler
open Ast.CmdHandler

let is_space = function
  | ' ' | '\t' -> true
  | _ -> false
;;

let space_p = satisfy is_space *> return () <?> "space_p"

(* Get rid of some number of zero or more spaces *)
let spaces_p = skip_many space_p <?> "spaces_p"

(* Get rid of some number of one or more spaces *)
let spaces1_p = skip_many1 space_p <?> "spaces1_p"
let trim_p p = spaces_p *> p <* spaces_p
let comma_p = trim_p (char ',') <?> "comma_p"

(* Parse a comment *)
let comment_p = char ';' *> skip_while (fun c -> not (Char.equal c '\n')) <?> "comment_p"

(* Parser that skips empty lines, spaces, and commets *)
let skip_empty_p =
  skip_many (space_p <|> end_of_line <|> comment_p) <?> "skip_empty_lines_p"
;;

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

(* Parse label, return it's name as a string *)
let label_str_p =
  (* The label must start with a letter *)
  lift2 (fun c s -> Char.escaped c ^ s) (satisfy is_letter) (take_while is_label_char)
  <?> "label_str_p"
;;

let label_p = label_str_p >>| (fun s -> Label s) <?> "label_p"

(* Parse an integer and return it as integer *)
let int_p =
  let sign_p = string "+" <|> string "-" in
  let const_str_p = lift2 ( ^ ) (option "" sign_p) (take_while1 is_digit) in
  const_str_p >>| int_of_string <?> "int_p"
;;

(* Generate parser for an integer *)
let gen_const_p int_is_t_const int_to_t_const =
  int_p
  >>= fun x ->
  if int_is_t_const x
  then return (Const (int_to_t_const x))
  else fail "Integer is too big"
;;

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
let xreg_name_p = gen_reg_name_p xmm_reg_name_list <?> "xreg_name_p"

(****************************************************************************************)
(* Generate parser for register names that returns Reg (...) *)
let gen_reg_p reg_name_p reg_name_to_t_reg =
  reg_name_p >>| fun reg_name -> Reg (reg_name_to_t_reg reg_name)
;;

(* Parse register name and convert them to Reg (...) *)
let breg_p = gen_reg_p breg_name_p reg_name_to_byte_reg <?> "breg_p"
let wreg_p = gen_reg_p wreg_name_p reg_name_to_word_reg <?> "wreg_p"
let dreg_p = gen_reg_p dreg_name_p reg_name_to_dword_reg <?> "dreg_p"
let xreg_p = gen_reg_p xreg_name_p reg_name_to_xmm_reg <?> "xreg_p"

(****************************************************************************************)
(* Generate parser for two registers that returns RegReg (...) *)
let gen_regreg_p reg_p =
  both (reg_p <* comma_p) reg_p
  >>| function
  | Reg r1, Reg r2 -> RegReg (r1, r2)
  | _ -> failwith "reg_p returned non-register"
;;

(* Parser two registers and convert them to RegReg (...) *)
let bregreg_p = gen_regreg_p breg_p <?> "bregreg_p"
let wregreg_p = gen_regreg_p wreg_p <?> "wregreg_p"
let dregreg_p = gen_regreg_p dreg_p <?> "dregreg_p"
let xregreg_p = gen_regreg_p xreg_p <?> "xregreg_p"

(****************************************************************************************)
(* Generate parser for register and constant that returns RegConst (...) *)

let gen_regconst_p reg_p const_p =
  both (reg_p <* comma_p) const_p
  >>| function
  | Reg r, Const x -> RegConst (r, x)
  (* This branch is not reachable if we did everything right *)
  | _ -> failwith "reg_p returned non-register or const_p returned non-constant"
;;

(* Parse register and constant and convert them to RegConst (...) *)
let bregconst_p = gen_regconst_p breg_p bconst_p <?> "bregconst_p"
let wregconst_p = gen_regconst_p wreg_p wconst_p <?> "wregconst_p"
let dregconst_p = gen_regconst_p dreg_p dconst_p <?> "dregconst_p"

(****************************************************************************************)
(* Generate a parser of one-line command *)
let gen_command_p operand_p converter cmd_str =
  string cmd_str *> spaces1_p *> operand_p >>| converter
;;

(****************************************************************************************)
(* Generate a parser of a string command *)
let gen_scommand_p cmd_str =
  gen_command_p label_p (fun x -> SCommand (scmd_str_to_command cmd_str x)) cmd_str
;;

(****************************************************************************************)
(* Generate a parser of a byte command *)
let gen_bcommand_p operand_p cmd_str_to_command cmd_str =
  gen_command_p operand_p (fun x -> BCommand (cmd_str_to_command cmd_str x)) cmd_str
;;

(* For now we'll consider all zero-args commands as BCommands *)
let gen_bcommand_zero_args_p cmd_str =
  (* We cannot use gen_bcommand_p since it wants at least one space after the command *)
  string cmd_str >>| fun _ -> BCommand (cmd_zero_args_str_to_command cmd_str)
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

(* Generate a parser of two-args dword command *)
let gen_dcommand_two_args_p =
  gen_dcommand_p (dregreg_p <|> dregconst_p) cmd_two_args_str_to_command
;;

(****************************************************************************************)
(* Generate a parser of an xmm command *)
let gen_xcommand_p operand_p cmd_str_to_command cmd_str =
  gen_command_p operand_p (fun x -> XCommand (cmd_str_to_command cmd_str x)) cmd_str
;;

(* Generate a parser of one-arg xmm command *)
let gen_xcommand_one_arg_p = gen_xcommand_p xreg_p xcmd_one_arg_str_to_command

(* Generate a parser of two-args xmm command *)
let gen_xcommand_two_args_p = gen_xcommand_p xregreg_p xcmd_two_args_str_to_command

(****************************************************************************************)
(* The following parsers are intended to parse a one-line command *)

(* Parse label declaration (e.g. "l1:") *)
let lcommand_p = label_str_p <* char ':' >>| (fun s -> LCommand s) <?> "lcommand_p"

let bcommand_p =
  choice
    (List.map gen_bcommand_zero_args_p cmd_zero_args_list
    @ List.map gen_bcommand_one_arg_p cmd_one_arg_list
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

let xcommand_p =
  choice
    (List.map gen_xcommand_one_arg_p xcmd_one_arg_list
    @ List.map gen_xcommand_two_args_p xcmd_two_args_list)
  <?> "xcommand_p"
;;

let scommand_p = choice (List.map gen_scommand_p scmd_list) <?> "scommand_p"

(****************************************************************************************)
(* Parse any instruction == line.
   We want dcommand_p to be the first so that all one-arg commands that take
   constants are parsed the same, i.e. "mul 5" should be parsed as DCommand.
   Otherwise, "mul 5" will be parsed as BCommand and "mul 500" will be parsed as WCommand *)
let instr_p =
  choice [ dcommand_p; wcommand_p; bcommand_p; xcommand_p; scommand_p; lcommand_p ]
;;

(* Parse the whole NASM program *)
let program_p =
  skip_empty_p
  *> sep_by1 (end_of_line *> skip_empty_p) (trim_p instr_p <* option () comment_p)
  <* skip_empty_p
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
      Printf.printf "Failed to parse %S%s" input e;
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

let ok_ast = test_ok pp_ast program_p
let fail_ast = test_fail pp_ast program_p

let%test _ =
  ok_ast
    "mov ax, bx\n     add eax, ecx"
    [ WCommand (Mov (RegReg (reg_name_to_word_reg "ax", reg_name_to_word_reg "bx")))
    ; DCommand (Add (RegReg (reg_name_to_dword_reg "eax", reg_name_to_dword_reg "ecx")))
    ]
;;

let%test _ =
  ok_ast
    "mov ax, bx\n     add eax, ecx\n inc ax  "
    [ WCommand (Mov (RegReg (reg_name_to_word_reg "ax", reg_name_to_word_reg "bx")))
    ; DCommand (Add (RegReg (reg_name_to_dword_reg "eax", reg_name_to_dword_reg "ecx")))
    ; WCommand (Inc (Reg (reg_name_to_word_reg "ax")))
    ]
;;

let%test _ =
  ok_ast
    "l1:\n mov ax, bx\n add eax, ecx\n inc bl\n l@abel2:   \n sub dh, 5\n\n\n\n   \n"
    [ LCommand "l1"
    ; WCommand (Mov (RegReg (reg_name_to_word_reg "ax", reg_name_to_word_reg "bx")))
    ; DCommand (Add (RegReg (reg_name_to_dword_reg "eax", reg_name_to_dword_reg "ecx")))
    ; BCommand (Inc (Reg (reg_name_to_byte_reg "bl")))
    ; LCommand "l@abel2"
    ; BCommand (Sub (RegConst (reg_name_to_byte_reg "dh", int_to_byte_const 5)))
    ]
;;

let%test _ = fail_ast "mov ax, bx   inc ax"
let%test _ = fail_ast "label_without_colon"

let%test _ =
  ok_ast
    {|l1:
       mov ax, bx
       je LAbEl$
        add eax, ecx
        inc bl
      l@abel2:
        sub dh, 5
         jmp l1

    |}
    [ LCommand "l1"
    ; WCommand (Mov (RegReg (reg_name_to_word_reg "ax", reg_name_to_word_reg "bx")))
    ; SCommand (Je (Label "LAbEl$"))
    ; DCommand (Add (RegReg (reg_name_to_dword_reg "eax", reg_name_to_dword_reg "ecx")))
    ; BCommand (Inc (Reg (reg_name_to_byte_reg "bl")))
    ; LCommand "l@abel2"
    ; BCommand (Sub (RegConst (reg_name_to_byte_reg "dh", int_to_byte_const 5)))
    ; SCommand (Jmp (Label "l1"))
    ]
;;

let%test _ = fail_ast "call eax, edx"

let%test _ =
  ok_ast
    {|
  ;comment
     l1:
       mov ax, bx
       je LAbEl$ ; something
       add eax, ecx
           ;comment on its own
       inc bl
     l@abel2: ; another comMem$nt
       sub dh, 5
       jmp l1
    ; Comment at the end
    |}
    [ LCommand "l1"
    ; WCommand (Mov (RegReg (reg_name_to_word_reg "ax", reg_name_to_word_reg "bx")))
    ; SCommand (Je (Label "LAbEl$"))
    ; DCommand (Add (RegReg (reg_name_to_dword_reg "eax", reg_name_to_dword_reg "ecx")))
    ; BCommand (Inc (Reg (reg_name_to_byte_reg "bl")))
    ; LCommand "l@abel2"
    ; BCommand (Sub (RegConst (reg_name_to_byte_reg "dh", int_to_byte_const 5)))
    ; SCommand (Jmp (Label "l1"))
    ]
;;

let%test _ =
  ok_ast
    {|l1:
        mov ax, bx
        push eax
        je LAbEl$
        add eax, ecx
        push 5
        inc bl
      l@abel2:
        sub dh, 5
        pop dx ; Actually we don't want to allow using
               ; push/pop with registers other than 32-bit
        jmp l1
    |}
    [ LCommand "l1"
    ; WCommand (Mov (RegReg (reg_name_to_word_reg "ax", reg_name_to_word_reg "bx")))
    ; DCommand (Push (Reg (reg_name_to_dword_reg "eax")))
    ; SCommand (Je (Label "LAbEl$"))
    ; DCommand (Add (RegReg (reg_name_to_dword_reg "eax", reg_name_to_dword_reg "ecx")))
    ; DCommand (Push (Const (int_to_dword_const 5)))
    ; BCommand (Inc (Reg (reg_name_to_byte_reg "bl")))
    ; LCommand "l@abel2"
    ; BCommand (Sub (RegConst (reg_name_to_byte_reg "dh", int_to_byte_const 5)))
    ; WCommand (Pop (Reg (reg_name_to_word_reg "dx")))
    ; SCommand (Jmp (Label "l1"))
    ]
;;

let%test _ = fail_ast "sub al, 1000"
let%test _ = fail_ast "mov ax, 1000000"

let%test _ =
  ok_ast
    "add edx, 1000000"
    [ DCommand (Add (RegConst (reg_name_to_dword_reg "edx", int_to_dword_const 1000000)))
    ]
;;

let%test _ =
  ok_ast
    {|l1:
        mov ax, bx
        push eax
        je LAbEl$
        add eax, ecx
        push 5
        inc bl
      l@abel2:
        sub dh, 5
        pop dx ; Actually we don't want to allow using
               ; push/pop with registers other than 32-bit
        cmp dx, 1
        jne l1
    |}
    [ LCommand "l1"
    ; WCommand (Mov (RegReg (reg_name_to_word_reg "ax", reg_name_to_word_reg "bx")))
    ; DCommand (Push (Reg (reg_name_to_dword_reg "eax")))
    ; SCommand (Je (Label "LAbEl$"))
    ; DCommand (Add (RegReg (reg_name_to_dword_reg "eax", reg_name_to_dword_reg "ecx")))
    ; DCommand (Push (Const (int_to_dword_const 5)))
    ; BCommand (Inc (Reg (reg_name_to_byte_reg "bl")))
    ; LCommand "l@abel2"
    ; BCommand (Sub (RegConst (reg_name_to_byte_reg "dh", int_to_byte_const 5)))
    ; WCommand (Pop (Reg (reg_name_to_word_reg "dx")))
    ; WCommand (Cmp (RegConst (reg_name_to_word_reg "dx", int_to_word_const 1)))
    ; SCommand (Jne (Label "l1"))
    ]
;;

let%test _ =
  ok_ast
    {|l1:
      ret
      call l1
    |}
    [ LCommand "l1"; BCommand Ret; SCommand (Call (Label "l1")) ]
;;

let%test _ =
  ok_ast
    {|mov eax, 9
      call fib
      jmp end
      fib:
        cmp eax, 0
        jne l1
        mov ebx, 0
        ret
      l1:
        cmp eax, 1
        jne l2
        mov ebx, 1
        ret
      l2:
        push eax
        sub eax, 2
        call fib
        pop eax
        push ebx
        sub eax, 1
        call fib
        pop ecx
        add ebx, ecx
        ret
      end:
    |}
    [ DCommand (Mov (RegConst (reg_name_to_dword_reg "eax", int_to_dword_const 9)))
    ; SCommand (Call (Label "fib"))
    ; SCommand (Jmp (Label "end"))
    ; LCommand "fib"
    ; DCommand (Cmp (RegConst (reg_name_to_dword_reg "eax", int_to_dword_const 0)))
    ; SCommand (Jne (Label "l1"))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "ebx", int_to_dword_const 0)))
    ; BCommand Ret
    ; LCommand "l1"
    ; DCommand (Cmp (RegConst (reg_name_to_dword_reg "eax", int_to_dword_const 1)))
    ; SCommand (Jne (Label "l2"))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "ebx", int_to_dword_const 1)))
    ; BCommand Ret
    ; LCommand "l2"
    ; DCommand (Push (Reg (reg_name_to_dword_reg "eax")))
    ; DCommand (Sub (RegConst (reg_name_to_dword_reg "eax", int_to_dword_const 2)))
    ; SCommand (Call (Label "fib"))
    ; DCommand (Pop (Reg (reg_name_to_dword_reg "eax")))
    ; DCommand (Push (Reg (reg_name_to_dword_reg "ebx")))
    ; DCommand (Sub (RegConst (reg_name_to_dword_reg "eax", int_to_dword_const 1)))
    ; SCommand (Call (Label "fib"))
    ; DCommand (Pop (Reg (reg_name_to_dword_reg "ecx")))
    ; DCommand (Add (RegReg (reg_name_to_dword_reg "ebx", reg_name_to_dword_reg "ecx")))
    ; BCommand Ret
    ; LCommand "end"
    ]
;;

let%test _ =
  ok_ast
    {| mov eax, 1
       mov ebx, 2
       mov ecx, 3
       mov edx, 4
       movdqa xmm0
       mov eax, 5
       movdqa xmm7
       addpd xmm0, xmm7
    |}
    [ DCommand (Mov (RegConst (reg_name_to_dword_reg "eax", int_to_dword_const 1)))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "ebx", int_to_dword_const 2)))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "ecx", int_to_dword_const 3)))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "edx", int_to_dword_const 4)))
    ; XCommand (Movdqa (Reg (reg_name_to_xmm_reg "xmm0")))
    ; DCommand (Mov (RegConst (reg_name_to_dword_reg "eax", int_to_dword_const 5)))
    ; XCommand (Movdqa (Reg (reg_name_to_xmm_reg "xmm7")))
    ; XCommand (Addpd (RegReg (reg_name_to_xmm_reg "xmm0", reg_name_to_xmm_reg "xmm7")))
    ]
;;

let%test _ = fail_ast {|mov xmm0, xmm1|}
let%test _ = fail_ast {|movdqa ax|}
let%test _ = fail_ast {|mulpd xmm3, 4|}
