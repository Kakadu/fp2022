open Ast
open Types

let rec str_of_terms terms = String.concat ", " (List.map str_of_term terms)

and str_of_term = function
  | Atomic (Ast.Num x) -> Base.string_of_int x
  | Atomic (Ast.Atom (Ast.Name x)) -> x
  | Atomic (Ast.Atom (Ast.Operator x)) -> x
  | Var x -> x
  | Compound { atom = Name "." } as x -> "[" ^ str_of_list x ^ "]"
  | Compound { atom = Name name; terms } -> name ^ "(" ^ str_of_terms terms ^ ")"
  | Compound { atom = Operator ","; terms = [ term1; term2 ] } ->
    "(" ^ str_of_term term1 ^ ", " ^ str_of_term term2 ^ ")"
  | Compound { atom = Operator name; terms = [ term1; term2 ] } ->
    str_of_term term1 ^ name ^ str_of_term term2
  | _ as x -> failwith ("Cannot get string of " ^ show_term x)

and str_of_list l =
  match l with
  | Compound { atom = Name "."; terms = [ l1; Atomic (Atom (Name "[]")) ] } ->
    str_of_term l1
  | Compound { atom = Name "."; terms = [ l1; Compound { atom; terms } ] }
    when equal_atom atom (Name ".") ->
    str_of_term l1 ^ ", " ^ str_of_list (Compound { atom; terms })
  | Compound { atom = Name "."; terms = [ l1; l2 ] } ->
    str_of_term l1 ^ ", " ^ str_of_term l2
  | _ -> failwith ("Cannot get string of " ^ show_term l)
;;

let rec print_substitution substitution =
  match substitution with
  | [ (t1, t2) ] -> Caml.Format.printf "%s = %s" (str_of_term t1) (str_of_term t2)
  | (t1, t2) :: tl ->
    Caml.Format.printf "%s = %s,\n" (str_of_term t1) (str_of_term t2);
    print_substitution tl
  | _ -> ()
;;

let print_choicepoints choicepoints =
  List.iter (fun cp -> Caml.Format.printf "%s\n" (show_choicepoint cp)) choicepoints
;;
