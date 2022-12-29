(** Copyright 2021-2022, Danila Pechenev & Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Interpret

let rec pp_value fmt =
  let rec print_list delimiter = function
    | [ head ] -> pp_value fmt head
    | head :: tail ->
      pp_value fmt head;
      fprintf fmt "%c " delimiter;
      print_list delimiter tail
    | [] -> ()
  in
  function
  | VInt value -> fprintf fmt "%d" value
  | VChar value -> fprintf fmt "%C" value
  | VBool value -> fprintf fmt "%B" value
  | VString value -> fprintf fmt "%S" value
  | VUnit -> fprintf fmt "()"
  | VList list ->
    fprintf fmt "[";
    print_list ';' list;
    fprintf fmt "]"
  | VTuple tuple ->
    fprintf fmt "(";
    print_list ',' tuple;
    fprintf fmt ")"
  | VADT (name, argument) ->
    fprintf fmt "%s " name;
    (match argument with
     | Some argument -> pp_value fmt argument
     | None -> ())
  | VEffectArg (name, _) | VEffectNoArg name | VEffectDeclaration name ->
    fprintf fmt "effect %s" name
  | VFun _ -> fprintf fmt "<fun>"
  | VEffectPattern _ -> fprintf fmt "effect pattern"
  | VEffectHandler (name, value) -> fprintf fmt "%s %a" name pp_value value
;;

let print_value value =
  let s = asprintf "%a" pp_value value in
  printf "%s\n" s
;;

let pp_error fmt = function
  | UnboundValue name -> fprintf fmt "Runtime error: unbound value %s." name
  | UnboundEffect name -> fprintf fmt "Runtime error: unbound effect %s." name
  | Unreachable ->
    fprintf
      fmt
      "This code is supposed to be unreachable. If you got this error, something must \
       have gone seriously wrong."
  | UnsupportedOperation -> fprintf fmt "Runtime error: unsupported operation."
  | Division_by_zero -> fprintf fmt "Runtime error: division by zero."
  | NotAFunction ->
    fprintf fmt "Runtime error: this is not a function, it cannot be applied."
  | TypeMismatch -> fprintf fmt "Runtime error: mismatching types."
  | MisusedWildcard ->
    fprintf
      fmt
      "Runtime error: wildcard must not appear on the right-hand side of an expression."
  | NotAnEffect -> fprintf fmt "Runtime error: this is not an effect."
  | PatternMatchingFailed -> fprintf fmt "Runtime error: pattern-matching failed."
  | NonExhaustivePatternMatching ->
    fprintf fmt "Runtime error: this pattern-matching is not exhaustive."
  | ContinuationFailure value -> fprintf fmt "Continuation failure %a" pp_value value
;;

let print_error error =
  let s = asprintf "%a" pp_error error in
  printf "%s\n" s
;;
