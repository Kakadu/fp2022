(** Copyright 2021-2022, Startsev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type asmreg8
type asmreg16
type asmreg32
type asmreg64
type asmreg128

type _ reg =
  | Reg8 : string -> asmreg8 reg
  | Reg16 : string -> asmreg16 reg
  | Reg32 : string -> asmreg32 reg
  | Reg64 : string -> asmreg64 reg
  | Reg128 : string -> asmreg128 reg

let show_reg : type a. a reg -> string = function
  | Reg8 x -> String.concat "" [ "(Reg8 \""; x; "\")" ]
  | Reg16 x -> String.concat "" [ "(Reg16 \""; x; "\")" ]
  | Reg32 x -> String.concat "" [ "(Reg32 \""; x; "\")" ]
  | Reg64 x -> String.concat "" [ "(Reg64 \""; x; "\")" ]
  | Reg128 x -> String.concat "" [ "(Reg128 \""; x; "\")" ]

type asmlabel
type _ const = ASMConst : string -> _ const
type _ asmvar = ASMVar : string -> _ asmvar
type _ label = ASMLabel : string -> asmlabel label

let show_label : type a. a label -> string = function
  | ASMLabel x -> String.concat "" [ "(ASMLabel \""; x; "\")" ]

let show_const : type a. a const -> string = function
  | ASMConst x -> String.concat "" [ "(ASMConst \""; x; "\")" ]

let show_asmvar : type a. a asmvar -> string = function
  | ASMVar x -> String.concat "" [ "(ASMVar \""; x; "\")" ]

type _ expr =
  | Add : 'a expr * 'a expr -> 'a expr
  | Sub : 'a expr * 'a expr -> 'a expr
  | Mul : 'a expr * 'a expr -> 'a expr
  | Div : 'a expr * 'a expr -> 'a expr
  | Reg : 'a reg -> 'a expr
  | Const : 'a const -> 'a expr
  | Var : 'a asmvar -> 'a expr
  | Label : 'a label -> 'a expr

let rec show_expr : type a. a expr -> string = function
  | Add (x, y) ->
      String.concat "" [ "(Add "; show_expr x; " "; show_expr y; ")" ]
  | Sub (x, y) ->
      String.concat "" [ "(Sub "; show_expr x; " "; show_expr y; ")" ]
  | Mul (x, y) ->
      String.concat "" [ "(Mul "; show_expr x; " "; show_expr y; ")" ]
  | Div (x, y) ->
      String.concat "" [ "(Div "; show_expr x; " "; show_expr y; ")" ]
  | Reg x -> String.concat "" [ "(Reg "; show_reg x; ")" ]
  | Const x -> String.concat "" [ "(Const "; show_const x; ")" ]
  | Var x -> String.concat "" [ "(Var "; show_asmvar x; ")" ]
  | Label x -> String.concat "" [ "(Label "; show_label x; ")" ]

type mnemonic = Mnemonic of string [@@deriving show { with_path = false }]

type command =
  | Args0 : mnemonic -> command
  | Args1 : mnemonic * 'a expr -> command
  | Args2 : mnemonic * 'a expr * 'a expr -> command

let show_command : command -> string = function
  | Args0 x -> String.concat "" [ "(Args0 "; show_mnemonic x; ")" ]
  | Args1 (x, y) ->
      String.concat "" [ "(Args1 "; show_mnemonic x; " "; show_expr y; ")" ]
  | Args2 (x, y, z) ->
      String.concat ""
        [ "(Args2 "; show_mnemonic x; " "; show_expr y; " "; show_expr z; ")" ]

type code_section = Command of command | Id of asmlabel label

let show_code_section : code_section -> string = function
  | Command x -> String.concat "" [ "(Command "; show_command x; ")" ]
  | Id x -> String.concat "" [ "(Id "; show_label x; ")" ]

type data_type = DB | DW | DD | DQ | DT
[@@deriving show { with_path = false }]

type var = string * data_type * string list
[@@deriving show { with_path = false }]

type dir = Code of code_section list | Data of var list

let show_dir : dir -> string = function
  | Code x ->
      String.concat ""
        [
          "[\n";
          List.fold_left
            (fun x y ->
              String.concat "" [ x; "\t"; show_code_section y; ";\n" ])
            "" x;
          "]";
        ]
  | Data x ->
      String.concat ""
        [
          "[\n";
          List.fold_left
            (fun x y -> String.concat "" [ x; "\t"; show_var y; "\n" ])
            "" x;
          "]";
        ]

type ast = dir list

let show_ast : ast -> string =
 fun x ->
  String.concat ""
    [
      "[\n";
      List.fold_left
        (fun x y -> String.concat "" [ x; "\t"; show_dir y; ";\n" ])
        "" x;
      "]";
    ]
