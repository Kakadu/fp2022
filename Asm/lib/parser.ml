(** Copyright 2021-2022, andreyizrailev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

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
  lift2 ( ^ ) (option "" sign_p) (take_while1 is_digit)
;;

let breg_p =
  choice
    [ string "ah"
    ; string "al"
    ; string "bh"
    ; string "bl"
    ; string "ch"
    ; string "cl"
    ; string "dh"
    ; string "dl"
    ]
;;

let wreg_p = choice [ string "ax"; string "bx"; string "cx"; string "dx" ]
let dreg_p = choice [ string "eax"; string "ebx"; string "ecx"; string "edx" ]
let reg_p = choice [ breg_p; wreg_p; dreg_p ]
let single_operand_p = choice [ reg_p; const_p ]
let mnem_one_arg_p = choice [ string "inc"; string "mul" ]

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
let%test _ = ok_string breg_p "ah" "ah"
let%test _ = fail_string breg_p "ax"
