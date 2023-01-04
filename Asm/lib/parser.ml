(** Copyright 2021-2023, andreyizrailev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast.Ast (MonadError.Result)
open Ast.CmdHandler (MonadError.Result)
open OperandsHandler.OperandsHandler (MonadError.Result)

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
  then (
    match int_to_t_const x with
    | Error e -> fail e
    | Ok c -> return (Const c))
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
let gen_reg_p reg_name_p reg_name_to_t_reg =
  reg_name_p
  >>= fun reg_name ->
  match reg_name_to_t_reg reg_name with
  | Error e -> fail e
  | Ok r -> return (Reg r)
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
  >>= function
  | Reg r1, Reg r2 -> return (RegReg (r1, r2))
  | _ -> fail "reg_p returned non-register"
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
  >>= function
  | Reg r, Const x -> return (RegConst (r, x))
  (* This branch is not reachable if we did everything right *)
  | _ -> fail "reg_p returned non-register or const_p returned non-constant"
;;

(* Parse register and constant and convert them to RegConst (...) *)
let bregconst_p = gen_regconst_p breg_p bconst_p <?> "bregconst_p"
let wregconst_p = gen_regconst_p wreg_p wconst_p <?> "wregconst_p"
let dregconst_p = gen_regconst_p dreg_p dconst_p <?> "dregconst_p"

(****************************************************************************************)
(* Generate a parser of one-line command *)
let gen_command_p operand_p cmd_str_to_comand gen_instr cmd_str =
  string cmd_str *> spaces1_p *> operand_p
  >>= fun x ->
  match cmd_str_to_comand cmd_str x with
  | Error e -> fail e
  | Ok v -> return (gen_instr v)
;;

(****************************************************************************************)
(* Generate a parser of a string command *)

let gen_scommand_p cmd_str = gen_command_p label_p scmd_str_to_command scommand cmd_str

(****************************************************************************************)
(* Generate a parser of a byte command *)
let gen_bcommand_p operand_p cmd_str_to_command cmd_str =
  gen_command_p operand_p cmd_str_to_command bcommand cmd_str
;;

(* For now we'll consider all zero-args commands as BCommands *)
let gen_bcommand_zero_args_p cmd_str =
  (* We cannot use gen_bcommand_p since it wants at least one space after the command *)
  string cmd_str
  >>= fun _ ->
  match cmd_zero_args_str_to_command cmd_str with
  | Error e -> fail e
  | Ok v -> return (BCommand v)
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
  gen_command_p operand_p cmd_str_to_command wcommand cmd_str
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
  gen_command_p operand_p cmd_str_to_command dcommand cmd_str
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
  gen_command_p operand_p cmd_str_to_command xcommand cmd_str
;;

(* Generate a parser of one-arg xmm command *)
let gen_xcommand_one_arg_p = gen_xcommand_p xreg_p xcmd_one_arg_str_to_command

(* Generate a parser of two-args xmm command *)
let gen_xcommand_two_args_p = gen_xcommand_p xregreg_p xcmd_two_args_str_to_command

(****************************************************************************************)
(* The following parsers are intended to parse a one-line command *)

(* Parse label declaration (e.g. "l1:") *)
let lcommand_p = label_str_p <* char ':' >>| lcommand <?> "lcommand_p"

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
open MonadError.Result

(* Taken from vs9h *)
let test_ok, test_fail =
  let ok ppf parser input expected =
    match expected with
    | Error e ->
      Printf.printf "%s" e;
      false
    | Ok expected ->
      (match parse_string ~consume:All parser input with
       | Ok res when expected = res -> true
       | Ok res ->
         ppf Format.std_formatter res;
         false
       | Error e ->
         Printf.printf "Failed to parse %S%s" input e;
         false)
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

let%test _ = ok_string (trim_p (string "test")) "    test " (return "test")
let%test _ = ok_string breg_name_p "ah" (return "ah")
let%test _ = fail_string breg_name_p "ax"

let ok_instruction = test_ok pp_instruction
let fail_instruction = test_fail pp_instruction

let%test _ =
  ok_instruction
    bcommand_p
    "inc ah"
    (let* ah = reg_name_to_byte_reg "ah" in
     return @@ BCommand (Inc (Reg ah)))
;;

let%test _ =
  ok_instruction
    wcommand_p
    "mul bx"
    (let* bx = reg_name_to_word_reg "bx" in
     return @@ WCommand (Mul (Reg bx)))
;;

let%test _ = fail_instruction dcommand_p "inc dl"

let%test _ =
  ok_instruction
    dcommand_p
    "mov ecx, edx"
    (let* ecx = reg_name_to_dword_reg "ecx" in
     let* edx = reg_name_to_dword_reg "edx" in
     return @@ DCommand (Mov (RegReg (ecx, edx))))
;;

let%test _ =
  ok_instruction
    wcommand_p
    "add bx, 1578"
    (let* bx = reg_name_to_word_reg "bx" in
     let* c = int_to_word_const 1578 in
     return @@ WCommand (Add (RegConst (bx, c))))
;;

let%test _ = fail_instruction bcommand_p "inc 314513245"
let%test _ = fail_instruction instr_p "add al, edx"
let%test _ = ok_instruction instr_p "abc?def$:" (return @@ LCommand "abc?def$")
let%test _ = fail_instruction instr_p "@abc:"

let ok_ast = test_ok pp_ast program_p
let fail_ast = test_fail pp_ast program_p

let%test _ =
  ok_ast
    "mov ax, bx\n     add eax, ecx"
    (let* ax = reg_name_to_word_reg "ax" in
     let* bx = reg_name_to_word_reg "bx" in
     let* eax = reg_name_to_dword_reg "eax" in
     let* ecx = reg_name_to_dword_reg "ecx" in
     return @@ [ WCommand (Mov (RegReg (ax, bx))); DCommand (Add (RegReg (eax, ecx))) ])
;;

let%test _ =
  ok_ast
    "mov ax, bx\n     add eax, ecx\n inc ax  "
    (let* ax = reg_name_to_word_reg "ax" in
     let* bx = reg_name_to_word_reg "bx" in
     let* eax = reg_name_to_dword_reg "eax" in
     let* ecx = reg_name_to_dword_reg "ecx" in
     return
     @@ [ WCommand (Mov (RegReg (ax, bx)))
        ; DCommand (Add (RegReg (eax, ecx)))
        ; WCommand (Inc (Reg ax))
        ])
;;

let%test _ =
  ok_ast
    "l1:\n mov ax, bx\n add eax, ecx\n inc bl\n l@abel2:   \n sub dh, 5\n\n\n\n   \n"
    (let* ax = reg_name_to_word_reg "ax" in
     let* bx = reg_name_to_word_reg "bx" in
     let* eax = reg_name_to_dword_reg "eax" in
     let* ecx = reg_name_to_dword_reg "ecx" in
     let* bl = reg_name_to_byte_reg "bl" in
     let* dh = reg_name_to_byte_reg "dh" in
     let* c5 = int_to_byte_const 5 in
     return
     @@ [ LCommand "l1"
        ; WCommand (Mov (RegReg (ax, bx)))
        ; DCommand (Add (RegReg (eax, ecx)))
        ; BCommand (Inc (Reg bl))
        ; LCommand "l@abel2"
        ; BCommand (Sub (RegConst (dh, c5)))
        ])
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
    (let* ax = reg_name_to_word_reg "ax" in
     let* bx = reg_name_to_word_reg "bx" in
     let* eax = reg_name_to_dword_reg "eax" in
     let* ecx = reg_name_to_dword_reg "ecx" in
     let* bl = reg_name_to_byte_reg "bl" in
     let* dh = reg_name_to_byte_reg "dh" in
     let* c5 = int_to_byte_const 5 in
     return
     @@ [ LCommand "l1"
        ; WCommand (Mov (RegReg (ax, bx)))
        ; SCommand (Je (Label "LAbEl$"))
        ; DCommand (Add (RegReg (eax, ecx)))
        ; BCommand (Inc (Reg bl))
        ; LCommand "l@abel2"
        ; BCommand (Sub (RegConst (dh, c5)))
        ; SCommand (Jmp (Label "l1"))
        ])
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
    (let* ax = reg_name_to_word_reg "ax" in
     let* bx = reg_name_to_word_reg "bx" in
     let* eax = reg_name_to_dword_reg "eax" in
     let* ecx = reg_name_to_dword_reg "ecx" in
     let* bl = reg_name_to_byte_reg "bl" in
     let* dh = reg_name_to_byte_reg "dh" in
     let* c5 = int_to_byte_const 5 in
     return
     @@ [ LCommand "l1"
        ; WCommand (Mov (RegReg (ax, bx)))
        ; SCommand (Je (Label "LAbEl$"))
        ; DCommand (Add (RegReg (eax, ecx)))
        ; BCommand (Inc (Reg bl))
        ; LCommand "l@abel2"
        ; BCommand (Sub (RegConst (dh, c5)))
        ; SCommand (Jmp (Label "l1"))
        ])
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
        pop dx ; Multi-line
               ; comment
        jmp l1
    |}
    (let* ax = reg_name_to_word_reg "ax" in
     let* bx = reg_name_to_word_reg "bx" in
     let* eax = reg_name_to_dword_reg "eax" in
     let* ecx = reg_name_to_dword_reg "ecx" in
     let* bl = reg_name_to_byte_reg "bl" in
     let* dh = reg_name_to_byte_reg "dh" in
     let* bc5 = int_to_byte_const 5 in
     let* dc5 = int_to_dword_const 5 in
     let* dx = reg_name_to_word_reg "dx" in
     return
     @@ [ LCommand "l1"
        ; WCommand (Mov (RegReg (ax, bx)))
        ; DCommand (Push (Reg eax))
        ; SCommand (Je (Label "LAbEl$"))
        ; DCommand (Add (RegReg (eax, ecx)))
        ; DCommand (Push (Const dc5))
        ; BCommand (Inc (Reg bl))
        ; LCommand "l@abel2"
        ; BCommand (Sub (RegConst (dh, bc5)))
        ; WCommand (Pop (Reg dx))
        ; SCommand (Jmp (Label "l1"))
        ])
;;

let%test _ = fail_ast "sub al, 1000"
let%test _ = fail_ast "mov ax, 1000000"

let%test _ =
  ok_ast
    "add edx, 1000000"
    (let* edx = reg_name_to_dword_reg "edx" in
     let* c = int_to_dword_const 1000000 in
     return @@ [ DCommand (Add (RegConst (edx, c))) ])
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
        pop dx ; Multi-line
               ; comment
        cmp dx, 1
        jne l1
    |}
    (let* ax = reg_name_to_word_reg "ax" in
     let* bx = reg_name_to_word_reg "bx" in
     let* eax = reg_name_to_dword_reg "eax" in
     let* ecx = reg_name_to_dword_reg "ecx" in
     let* bl = reg_name_to_byte_reg "bl" in
     let* dh = reg_name_to_byte_reg "dh" in
     let* bc5 = int_to_byte_const 5 in
     let* dc5 = int_to_dword_const 5 in
     let* dx = reg_name_to_word_reg "dx" in
     let* wc1 = int_to_word_const 1 in
     return
     @@ [ LCommand "l1"
        ; WCommand (Mov (RegReg (ax, bx)))
        ; DCommand (Push (Reg eax))
        ; SCommand (Je (Label "LAbEl$"))
        ; DCommand (Add (RegReg (eax, ecx)))
        ; DCommand (Push (Const dc5))
        ; BCommand (Inc (Reg bl))
        ; LCommand "l@abel2"
        ; BCommand (Sub (RegConst (dh, bc5)))
        ; WCommand (Pop (Reg dx))
        ; WCommand (Cmp (RegConst (dx, wc1)))
        ; SCommand (Jne (Label "l1"))
        ])
;;

let%test _ =
  ok_ast
    {|l1:
      ret
      call l1
    |}
    (return @@ [ LCommand "l1"; BCommand Ret; SCommand (Call (Label "l1")) ])
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
    (let* eax = reg_name_to_dword_reg "eax" in
     let* ebx = reg_name_to_dword_reg "ebx" in
     let* ecx = reg_name_to_dword_reg "ecx" in
     let* c0 = int_to_dword_const 0 in
     let* c1 = int_to_dword_const 1 in
     let* c2 = int_to_dword_const 2 in
     let* c9 = int_to_dword_const 9 in
     return
     @@ [ DCommand (Mov (RegConst (eax, c9)))
        ; SCommand (Call (Label "fib"))
        ; SCommand (Jmp (Label "end"))
        ; LCommand "fib"
        ; DCommand (Cmp (RegConst (eax, c0)))
        ; SCommand (Jne (Label "l1"))
        ; DCommand (Mov (RegConst (ebx, c0)))
        ; BCommand Ret
        ; LCommand "l1"
        ; DCommand (Cmp (RegConst (eax, c1)))
        ; SCommand (Jne (Label "l2"))
        ; DCommand (Mov (RegConst (ebx, c1)))
        ; BCommand Ret
        ; LCommand "l2"
        ; DCommand (Push (Reg eax))
        ; DCommand (Sub (RegConst (eax, c2)))
        ; SCommand (Call (Label "fib"))
        ; DCommand (Pop (Reg eax))
        ; DCommand (Push (Reg ebx))
        ; DCommand (Sub (RegConst (eax, c1)))
        ; SCommand (Call (Label "fib"))
        ; DCommand (Pop (Reg ecx))
        ; DCommand (Add (RegReg (ebx, ecx)))
        ; BCommand Ret
        ; LCommand "end"
        ])
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
    (let* eax = reg_name_to_dword_reg "eax" in
     let* ebx = reg_name_to_dword_reg "ebx" in
     let* ecx = reg_name_to_dword_reg "ecx" in
     let* edx = reg_name_to_dword_reg "edx" in
     let* xmm0 = reg_name_to_xmm_reg "xmm0" in
     let* xmm7 = reg_name_to_xmm_reg "xmm7" in
     let* c1 = int_to_dword_const 1 in
     let* c2 = int_to_dword_const 2 in
     let* c3 = int_to_dword_const 3 in
     let* c4 = int_to_dword_const 4 in
     let* c5 = int_to_dword_const 5 in
     return
     @@ [ DCommand (Mov (RegConst (eax, c1)))
        ; DCommand (Mov (RegConst (ebx, c2)))
        ; DCommand (Mov (RegConst (ecx, c3)))
        ; DCommand (Mov (RegConst (edx, c4)))
        ; XCommand (Movdqa (Reg xmm0))
        ; DCommand (Mov (RegConst (eax, c5)))
        ; XCommand (Movdqa (Reg xmm7))
        ; XCommand (Addpd (RegReg (xmm0, xmm7)))
        ])
;;

let%test _ = fail_ast {|mov xmm0, xmm1|}
let%test _ = fail_ast {|movdqa ax|}
let%test _ = fail_ast {|mulpd xmm3, 4|}

(* Calculate (1, 2, 3) x ((4, 5, 6), (7, 8, 9), (10, 11, 12)) *)
let%test _ =
  ok_ast
    {| mov eax, 1
       mov ebx, 1
       mov ecx, 1
       movdqa xmm0
       mov eax, 2
       mov ebx, 2
       mov ecx, 2
       movdqa xmm1
       mov eax, 3
       mov ebx, 3
       mov ecx, 3
       movdqa xmm2
       mov eax, 4
       mov ebx, 5
       mov ecx, 6
       movdqa xmm3
       mov eax, 7
       mov ebx, 8
       mov ecx, 9
       movdqa xmm4
       mov eax, 10
       mov ebx, 11
       mov ecx, 12
       movdqa xmm5
       mulpd xmm0, xmm3
       mulpd xmm1, xmm4
       mulpd xmm2, xmm5
       addpd xmm0, xmm1
       addpd xmm0, xmm2
    |}
    (let* eax = reg_name_to_dword_reg "eax" in
     let* ebx = reg_name_to_dword_reg "ebx" in
     let* ecx = reg_name_to_dword_reg "ecx" in
     let* xmm0 = reg_name_to_xmm_reg "xmm0" in
     let* xmm1 = reg_name_to_xmm_reg "xmm1" in
     let* xmm2 = reg_name_to_xmm_reg "xmm2" in
     let* xmm3 = reg_name_to_xmm_reg "xmm3" in
     let* xmm4 = reg_name_to_xmm_reg "xmm4" in
     let* xmm5 = reg_name_to_xmm_reg "xmm5" in
     let* c1 = int_to_dword_const 1 in
     let* c2 = int_to_dword_const 2 in
     let* c3 = int_to_dword_const 3 in
     let* c4 = int_to_dword_const 4 in
     let* c5 = int_to_dword_const 5 in
     let* c6 = int_to_dword_const 6 in
     let* c7 = int_to_dword_const 7 in
     let* c8 = int_to_dword_const 8 in
     let* c9 = int_to_dword_const 9 in
     let* c10 = int_to_dword_const 10 in
     let* c11 = int_to_dword_const 11 in
     let* c12 = int_to_dword_const 12 in
     return
     @@ [ DCommand (Mov (RegConst (eax, c1)))
        ; DCommand (Mov (RegConst (ebx, c1)))
        ; DCommand (Mov (RegConst (ecx, c1)))
        ; XCommand (Movdqa (Reg xmm0))
        ; DCommand (Mov (RegConst (eax, c2)))
        ; DCommand (Mov (RegConst (ebx, c2)))
        ; DCommand (Mov (RegConst (ecx, c2)))
        ; XCommand (Movdqa (Reg xmm1))
        ; DCommand (Mov (RegConst (eax, c3)))
        ; DCommand (Mov (RegConst (ebx, c3)))
        ; DCommand (Mov (RegConst (ecx, c3)))
        ; XCommand (Movdqa (Reg xmm2))
        ; DCommand (Mov (RegConst (eax, c4)))
        ; DCommand (Mov (RegConst (ebx, c5)))
        ; DCommand (Mov (RegConst (ecx, c6)))
        ; XCommand (Movdqa (Reg xmm3))
        ; DCommand (Mov (RegConst (eax, c7)))
        ; DCommand (Mov (RegConst (ebx, c8)))
        ; DCommand (Mov (RegConst (ecx, c9)))
        ; XCommand (Movdqa (Reg xmm4))
        ; DCommand (Mov (RegConst (eax, c10)))
        ; DCommand (Mov (RegConst (ebx, c11)))
        ; DCommand (Mov (RegConst (ecx, c12)))
        ; XCommand (Movdqa (Reg xmm5))
        ; XCommand (Mulpd (RegReg (xmm0, xmm3)))
        ; XCommand (Mulpd (RegReg (xmm1, xmm4)))
        ; XCommand (Mulpd (RegReg (xmm2, xmm5)))
        ; XCommand (Addpd (RegReg (xmm0, xmm1)))
        ; XCommand (Addpd (RegReg (xmm0, xmm2)))
        ])
;;
