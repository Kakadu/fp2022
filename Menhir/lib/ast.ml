(*
type token = string
type rule_name = string

type rule_component =
  | Token of token
  | Rule_name of rule_name

type rule = rule_name * rule_component list

type tree =
  | Node_rule of rule * tree list
  | Leaf_token of token
*)
(*tree than string*)

(* 
  for expr; PLUS; expr, for example, we should have something like this:
    Printf.sprintf "[expr: %s PLUS %s]" (expr_to_string e1) (expr_to_string e2)
*)

(*
  let print_tree : (tree -> string) = function 
    ...
*)

(* result will probably be a function that takes 
the string representation of the expr 
parameters and returns the result string *)
(*
type mly_type =
  { tokens : token list
  ; start_rule : rule_name
  ; rules : rule list
  }
*)

type grammar = string * (string * string list) list

(* String set. Note that set is a functor building an implementation of the set structure given a TOTALLY ORDERED TYPE, so everything is okay there. *)
module SymbolSet = Set.Make (String)

(* Map where keys are strings. *)
module SMap = Map.Make (String)

type symbolMap = SymbolSet.t SMap.t

type parseTree =
  | Term of string
  | Nonterm of string * parseTree list
