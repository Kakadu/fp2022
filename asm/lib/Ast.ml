(** Copyright 2021-2022, Startsev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** label to use in code section such myLabel: and to use as argument of jmp commands *)
type label = Label of string [@@deriving show { with_path = false }]

(** arithmetic expression or register to use as argument *)
type expr =
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Const of string  (** interger constant *)
  | Var of string  (** global constant *)
  | Reg of string  (** comman rigister *)
  | Lab of label  (** labels for jmps *)
[@@deriving show { with_path = false }]

(** just name of command *)
type mnemonic = Mnemonic of string [@@deriving show { with_path = false }]

(** command with her arguments *)
type command =
  | Args0 of mnemonic
  | Args1 of mnemonic * expr
  | Args2 of mnemonic * expr * expr
[@@deriving show { with_path = false }]

(** one line of code incode section that contains command and her arguments or label to jump *)
type code_section = Command of command | Id of label
[@@deriving show { with_path = false }]

(** data type to use in data section *)
type data_type = DataType of string [@@deriving show { with_path = false }]

(** one line of data declaration in data section that containd name of constant her type and value *)
type var = Variable of string * data_type * string list
[@@deriving show { with_path = false }]

(** main ast that contains code section with list of commands and data section with list of data declarations *)
type ast = Code of code_section list | Data of var list
[@@deriving show { with_path = false }]
