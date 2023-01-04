(** Copyright 2021-2022, Danila Pechenev & Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Interpret

let rec pp_value fmt =
  let pp_list fmt delimiter =
    pp_print_list
      ~pp_sep:(fun fmt _ -> fprintf fmt delimiter)
      (fun fmt value -> pp_value fmt value)
      fmt
  in
  function
  | VInt value -> fprintf fmt "%d" value
  | VChar value -> fprintf fmt "%C" value
  | VBool value -> fprintf fmt "%B" value
  | VString value -> fprintf fmt "%S" value
  | VUnit -> fprintf fmt "()"
  | VList list -> fprintf fmt "[%a]" (fun fmt -> pp_list fmt "; ") list
  | VTuple tuple -> fprintf fmt "(%a)" (fun fmt -> pp_list fmt ", ") tuple
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

let print_value = printf "%a" pp_value

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

let print_error = printf "%a" pp_error

let%expect_test _ =
  print_value @@ VTuple [ VChar 'f'; VInt 0 ];
  [%expect "('f', 0)"]
;;

let%expect_test _ =
  print_value @@ VADT ("Some", Some (VInt 2));
  [%expect "Some 2"]
;;

let%expect_test _ =
  print_value @@ VList [ VInt 1; VInt 2 ];
  [%expect "[1; 2]"]
;;

let%expect_test _ =
  print_error @@ NonExhaustivePatternMatching;
  [%expect "Runtime error: this pattern-matching is not exhaustive."]
;;

let%expect_test _ =
  print_error @@ Division_by_zero;
  [%expect "Runtime error: division by zero."]
;;
