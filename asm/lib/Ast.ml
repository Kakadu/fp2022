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

type dyn_reg = Dyn : 'a reg -> dyn_reg [@@unboxed]
type const = ASMConst : string -> const
type asmvar = ASMVar : string -> asmvar
type label = ASMLabel : string -> label

type expr =
  | Add : expr * expr -> expr
  | Sub : expr * expr -> expr
  | Mul : expr * expr -> expr
  | Div : expr * expr -> expr
  | Const : const -> expr
  | Var : asmvar -> expr

type double_arg =
  | RegToReg : 'a reg * 'a reg -> double_arg
  | RegToExpr : _ reg * expr -> double_arg

type single_arg = Reg : _ reg -> single_arg | Label : label -> single_arg

type mnemonic =
  | RET
  | SYSCALL
  | PUSH of single_arg
  | POP of single_arg
  | INC of single_arg
  | DEC of single_arg
  | NOT of single_arg
  | NEG of single_arg
  | JMP of single_arg
  | JE of single_arg
  | JNE of single_arg
  | JZ of single_arg
  | JG of single_arg
  | JGE of single_arg
  | JL of single_arg
  | JLE of single_arg
  | MOV of double_arg
  | ADD of double_arg
  | SUB of double_arg
  | IMUL of double_arg
  | AND of double_arg
  | XOR of double_arg
  | OR of double_arg
  | SHL of double_arg
  | SHR of double_arg
  | CMP of double_arg

type code_section = Command of mnemonic | Id of label

type data_type = DB | DW | DD | DQ | DT
[@@deriving show { with_path = false }]

type var = Variable of string * data_type * string list
[@@deriving show { with_path = false }]

type dir = Code of code_section list | Data of var list
type ast = Ast of dir list

let show_reg : type a. a reg -> string = function
  | Reg8 x -> String.concat "" [ "(Reg8 \""; x; "\")" ]
  | Reg16 x -> String.concat "" [ "(Reg16 \""; x; "\")" ]
  | Reg32 x -> String.concat "" [ "(Reg32 \""; x; "\")" ]
  | Reg64 x -> String.concat "" [ "(Reg64 \""; x; "\")" ]
  | Reg128 x -> String.concat "" [ "(Reg128 \""; x; "\")" ]

let show_dyn_reg (Dyn x) = String.concat "" [ "(Dyn "; show_reg x; ")" ]

let show_label : label -> string = function
  | ASMLabel x -> String.concat "" [ "(ASMLabel \""; x; "\")" ]

let show_const : const -> string = function
  | ASMConst x -> String.concat "" [ "(ASMConst \""; x; "\")" ]

let show_asmvar : asmvar -> string = function
  | ASMVar x -> String.concat "" [ "(ASMVar \""; x; "\")" ]

let rec show_expr : expr -> string = function
  | Add (x, y) ->
      String.concat "" [ "(Add "; show_expr x; " "; show_expr y; ")" ]
  | Sub (x, y) ->
      String.concat "" [ "(Sub "; show_expr x; " "; show_expr y; ")" ]
  | Mul (x, y) ->
      String.concat "" [ "(Mul "; show_expr x; " "; show_expr y; ")" ]
  | Div (x, y) ->
      String.concat "" [ "(Div "; show_expr x; " "; show_expr y; ")" ]
  | Const x -> String.concat "" [ "(Const "; show_const x; ")" ]
  | Var x -> String.concat "" [ "(Var "; show_asmvar x; ")" ]

let show_double_arg = function
  | RegToReg (x, y) ->
      String.concat "" [ "(RegToReg "; show_reg x; " "; show_reg y; ")" ]
  | RegToExpr (x, y) ->
      String.concat "" [ "(RegToExpr "; show_reg x; " "; show_expr y; ")" ]

let show_single_arg = function
  | Reg x -> String.concat "" [ "(Reg "; show_reg x; ")" ]
  | Label x -> String.concat "" [ "(Label "; show_label x; ")" ]

let show_mnemonic : mnemonic -> string = function
  | RET -> "(RET)"
  | SYSCALL -> "(SYSCALL)"
  | PUSH x -> String.concat "" [ "(PUSH "; show_single_arg x; ")" ]
  | POP x -> String.concat "" [ "(POP "; show_single_arg x; ")" ]
  | INC x -> String.concat "" [ "(INC "; show_single_arg x; ")" ]
  | DEC x -> String.concat "" [ "(DEC "; show_single_arg x; ")" ]
  | NOT x -> String.concat "" [ "(NOT "; show_single_arg x; ")" ]
  | NEG x -> String.concat "" [ "(NEG "; show_single_arg x; ")" ]
  | JMP x -> String.concat "" [ "(JMP "; show_single_arg x; ")" ]
  | JE x -> String.concat "" [ "(JE "; show_single_arg x; ")" ]
  | JNE x -> String.concat "" [ "(JNE "; show_single_arg x; ")" ]
  | JZ x -> String.concat "" [ "(JZ "; show_single_arg x; ")" ]
  | JG x -> String.concat "" [ "(JG "; show_single_arg x; ")" ]
  | JGE x -> String.concat "" [ "(JGE "; show_single_arg x; ")" ]
  | JL x -> String.concat "" [ "(JL "; show_single_arg x; ")" ]
  | JLE x -> String.concat "" [ "(JLE "; show_single_arg x; ")" ]
  | MOV x -> String.concat "" [ "(MOV "; show_double_arg x; ")" ]
  | ADD x -> String.concat "" [ "(ADD "; show_double_arg x; ")" ]
  | SUB x -> String.concat "" [ "(SUB "; show_double_arg x; ")" ]
  | IMUL x -> String.concat "" [ "(IMUL "; show_double_arg x; ")" ]
  | AND x -> String.concat "" [ "(AND "; show_double_arg x; ")" ]
  | XOR x -> String.concat "" [ "(XOR "; show_double_arg x; ")" ]
  | OR x -> String.concat "" [ "(OR "; show_double_arg x; ")" ]
  | SHL x -> String.concat "" [ "(SHL "; show_double_arg x; ")" ]
  | SHR x -> String.concat "" [ "(SHR "; show_double_arg x; ")" ]
  | CMP x -> String.concat "" [ "(CMP "; show_double_arg x; ")" ]

let show_code_section : code_section -> string = function
  | Command x -> String.concat "" [ "(Command "; show_mnemonic x; ")" ]
  | Id x -> String.concat "" [ "(Id "; show_label x; ")" ]

let show_dir : dir -> string = function
  | Code x ->
      String.concat ""
        [
          "Code ";
          "[\n";
          List.fold_left
            (fun x y ->
              String.concat "" [ x; "\t\t"; show_code_section y; ";\n" ])
            "" x;
          "]";
        ]
  | Data x ->
      String.concat ""
        [
          "Data ";
          "[\n";
          List.fold_left
            (fun x y -> String.concat "" [ x; "\t\t"; show_var y; "\n" ])
            "" x;
          "]";
        ]

let show_ast : ast -> string =
 fun (Ast x) ->
  String.concat ""
    [
      "(Ast ";
      "[\n";
      List.fold_left
        (fun x y -> String.concat "" [ x; "\t"; show_dir y; ";\n" ])
        "" x;
      "]";
    ]
