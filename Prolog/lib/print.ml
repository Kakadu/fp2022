(** Copyright 2021-2022, Ilya Shchuckin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Types
open Types.Result

let rec str_of_terms terms =
  let rec get_str_list = function
    | hd :: tl ->
      str_of_term hd >>= fun str -> get_str_list tl >>| fun list -> str :: list
    | _ -> return []
  in
  match get_str_list terms with
  | Ok list -> return (String.concat ", " list)
  | Error _ as x -> x

and str_of_term_pair (t1, t2) =
  str_of_term t1 >>= fun str1 -> str_of_term t2 >>| fun str2 -> str1, str2

and str_of_term = function
  | Atomic (Ast.Num x) -> return (Int.to_string x)
  | Atomic (Ast.Atom (Ast.Name x)) -> return x
  | Atomic (Ast.Atom (Ast.Operator x)) -> return x
  | Var x -> return x
  | Compound { atom = Name "." } as x ->
    str_of_list x >>| fun str -> String.concat "" [ "["; str; "]" ]
  | Compound { atom = Name name; terms } ->
    str_of_terms terms >>| fun str -> String.concat "" [ name; "("; str; ")" ]
  | Compound { atom = Operator ","; terms = [ term1; term2 ] } ->
    str_of_term_pair (term1, term2)
    >>| fun (str1, str2) -> String.concat "" [ "("; str1; ", "; str2; ")" ]
  | Compound { atom = Operator name; terms = [ term1; term2 ] } ->
    str_of_term_pair (term1, term2)
    >>| fun (str1, str2) -> String.concat "" [ str1; name; str2 ]
  | x ->
    fail (Critical (String.concat "" [ "Cannot convert "; show_term x; " to string" ]))

(** Converts compound term with [atom = Name "."] to string.  *)
and str_of_list l =
  match l with
  | Compound { atom = Name "."; terms = [ l1; Atomic (Atom (Name "[]")) ] } ->
    str_of_term l1
  | Compound { atom = Name "."; terms = [ l1; Compound { atom; terms } ] }
    when equal_atom atom (Name ".") ->
    str_of_term l1
    >>= fun str1 ->
    str_of_list (Compound { atom; terms })
    >>| fun str2 -> String.concat ", " [ str1; str2 ]
  | Compound { atom = Name "."; terms = [ l1; l2 ] } ->
    str_of_term_pair (l1, l2) >>| fun (str1, str2) -> String.concat ", " [ str1; str2 ]
  | _ ->
    fail (Critical (String.concat "" [ "Cannot convert "; show_term l; " to string" ]))
;;

let print_substitution sub =
  let rec get_str_list = function
    | hd :: tl ->
      str_of_term_pair hd
      >>= fun (left, right) ->
      get_str_list tl >>| fun list -> String.concat " = " [ left; right ] :: list
    | _ -> return []
  in
  match get_str_list sub with
  | Ok lines -> Caml.Format.printf "%s" (String.concat ",\n" lines)
  | Error _ -> Caml.Format.printf "Error: Cannot print substitution\n"
;;

let print_choicepoints choicepoints =
  List.iter (fun cp -> Caml.Format.printf "%s\n" (show_choicepoint cp)) choicepoints
;;
