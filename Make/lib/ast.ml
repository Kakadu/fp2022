(** Copyright 2021-2022, ol-imorozko and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base

type substitute =
  | Regular of substitute list
  (* Regular substitution, $(var_name).
     Could be recursive, like a = b, b = c, $($(a)) = $(b) = c *)
  | Pattern (* Pattern substitution, % *)
  | None of string (* No substitution *)
  | Asterisk (* The stem with which an implicit rule matches, $* *)
  | At (* The file name of the target of the rule, $@ *)
  | Lesser (* The name of the first prerequisite, $< *)
[@@deriving show { with_path = false }, variants, sexp, compare]

(* Used for representation of a strings containing substitutions.
 For example, string "$(a)_xyz_$(xy)-%" is a substitute list
 [Regular a; None "_xyz_"; Regular xy; None "-"; Pattern]
 *)
type word = substitute list [@@deriving show { with_path = false }, sexp, compare]

(* variable name * variable value *)
type var =
  | Recursive of word * word list (* Recursively expanded variable (=) *)
  | Simply of word * word list (* Simply expanded variable (:=) *)
  | Conditional of word * word list (* Conditional variable assignment (?=) *)
[@@deriving show { with_path = false }, variants]

type recipe =
  | Echo of word
  | Silent of word
[@@deriving show { with_path = false }, variants, sexp]

(* <target> [<target[s]>...]: [<prerequisite[s]>...]
    [<recipe[s]>...]
 *)
type rule =
  { targets : word * word list
  ; prerequisites : word list
  ; recipes : recipe list
  }
[@@deriving show { with_path = false }]

type expr =
  | Rule of rule
  | Var of var
[@@deriving show { with_path = false }]

(** Ast for the Make syntax

   Makefile, essentially, is an expr list. However, is should contain at least one rule.
   We're ensure it by extracting that rule from the list. But we need to remember it's
   position in the list as this is important when this rule contains a substitute variable.
 *)
type ast = (rule * int) * expr list [@@deriving show { with_path = false }]
