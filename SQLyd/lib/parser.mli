(** Copyright 2022-2023, Danil Yevdokimov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Parser analyzes and executes commands based on the ouput of the tokenizer *)

open Type

exception Malformed of string
exception Empty

(* partition the line into commands and get rid of ";" *)
val parse : database -> string -> database

(* parse one command and call the appropriate parse function based on what type of command it is *)
val parse_query : database -> token list -> database

(* parse a create database command to send to controller_create to create a table *)
val parse_create : database -> token list -> database
val parse_insert : database -> token list -> database
val parse_delete : database -> token list -> database
val parse_update : database -> token list -> database
val parse_select : database -> token list -> database
val parse_drop : database -> token list -> database

(* partial function that takes condition expression and a pair_data
    Return true if data satisfy condition tokens and false otherwise
    Requires condition expression to be token type but each must be one of expr type, and pair_data must
    have data with same length, column value must be in String and data 
    value must be in the corrisponidng String or Int or Float *)
val parse_where : token list -> string list * string list -> bool

(* saves proposed table as csv files *)
val parse_save : database -> token list -> database

(* exposed helper to test, starting below, comment out when deploy *)
val expressions_or : expr_type list -> expr_type list list

(* Evaluate AND relationships *)
val and_condition_evaluater
  :  expr_type
  -> expr_type
  -> expr_type
  -> (expr_type * expr_type) list
  -> bool

(* Evaluate OR relationships *)
val evaluate_or : expr_type list list -> (expr_type * expr_type) list -> bool
