(** Copyright 2021-2022, Startsev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type asmreg8
type asmreg16
type asmreg32
type asmreg64
type asmreg128

(** Normal regs *)
type _ reg =
  | Reg8 : string -> asmreg8 reg
  | Reg16 : string -> asmreg16 reg
  | Reg32 : string -> asmreg32 reg
  | Reg64 : string -> asmreg64 reg

(** Regs with sse regs *)
type _ reg_e =
  | Reg8 : string -> asmreg8 reg_e
  | Reg16 : string -> asmreg16 reg_e
  | Reg32 : string -> asmreg32 reg_e
  | Reg64 : string -> asmreg64 reg_e
  | Reg128 : string -> asmreg128 reg_e

(** Types for some functions that returns _ reg ot _ reg_e *)
type dyn_reg_e = Dyn_e : 'a reg_e -> dyn_reg_e [@@unboxed]

type dyn_reg = Dyn : 'a reg -> dyn_reg [@@unboxed]

(** Consts in arithmetic expression *)
type const = ASMConst : string -> const

(** Global consts *)
type asmvar = ASMVar : string -> asmvar

(** Labels used as an argument and in the declaration of labels *)
type label = ASMLabel : string -> label

(** Arithmetic expression *)
type expr =
  | Add : expr * expr -> expr
  | Sub : expr * expr -> expr
  | Mul : expr * expr -> expr
  | Div : expr * expr -> expr
  | Const : const -> expr
  | Var : asmvar -> expr

(** Type that contains all combinations of arguments *)
type double_arg =
  | RegToReg : 'a reg_e * 'a reg_e -> double_arg
  | RegToExpr : _ reg * expr -> double_arg
  | RegToVar : asmreg128 reg_e * asmvar -> double_arg

(** All possible mnemonics and their arguments *)
type mnemonic =
  | RET
  | PUSH : _ reg -> mnemonic
  | POP : _ reg -> mnemonic
  | INC : _ reg -> mnemonic
  | DEC : _ reg -> mnemonic
  | NOT : _ reg -> mnemonic
  | NEG : _ reg -> mnemonic
  | JMP of label
  | JE of label
  | JNE of label
  | JZ of label
  | JG of label
  | JGE of label
  | JL of label
  | JLE of label
  | MOV of double_arg
  | ADD of double_arg
  | SUB of double_arg
  | IMUL of double_arg
  | AND of double_arg
  | XOR of double_arg
  | OR of double_arg
  | SHL : _ reg_e * expr -> mnemonic
  | SHR : _ reg_e * expr -> mnemonic
  | CMP of double_arg

(** One line of code in code section *)
type code_section = Command of mnemonic | Id of label

(** Data type for global consts *)
type data_type = DB | DW | DD | DQ | DT
[@@deriving show { with_path = false }]

(** Possible declaration of global consts*)
type value = Num of string list | Str of string
[@@deriving show { with_path = false }]

(** Global const *)
type var = Variable of string * data_type * value
[@@deriving show { with_path = false }]

(** Code or data directory *)
type dir = Code of code_section list | Data of var list

(** Main ast *)
type ast = Ast of dir list

(****************************show functions****************************************)
let show_reg : type a. a reg -> string = function
  | Reg8 x -> Printf.sprintf {|(Reg8 "%s")|} x
  | Reg16 x -> Printf.sprintf {|(Reg16 "%s")|} x
  | Reg32 x -> Printf.sprintf {|(Reg32 "%s")|} x
  | Reg64 x -> Printf.sprintf {|(Reg64 "%s")|} x

let show_reg_e : type a. a reg_e -> string = function
  | Reg8 x -> Printf.sprintf {|(Reg8 "%s")|} x
  | Reg16 x -> Printf.sprintf {|(Reg16 "%s")|} x
  | Reg32 x -> Printf.sprintf {|(Reg32 "%s")|} x
  | Reg64 x -> Printf.sprintf {|(Reg64 "%s")|} x
  | Reg128 x -> Printf.sprintf {|(Reg128 "%s")|} x

let show_dyn_reg_e : dyn_reg_e -> string =
 fun (Dyn_e x) -> Printf.sprintf {|(Dyn_e "%s")|} (show_reg_e x)

let show_dyn_reg : dyn_reg -> string =
 fun (Dyn x) -> Printf.sprintf {|(Dyn "%s")|} (show_reg x)

let show_label : label -> string = function
  | ASMLabel x -> Printf.sprintf {|(ASMLabel "%s")|} x

let show_const : const -> string = function
  | ASMConst x -> Printf.sprintf {|(ASMConst "%s")|} x

let show_asmvar : asmvar -> string = function
  | ASMVar x -> Printf.sprintf {|(ASMVar "%s")|} x

let rec show_expr : expr -> string = function
  | Add (x, y) -> Printf.sprintf {|(Add (%s, %s))|} (show_expr x) (show_expr y)
  | Sub (x, y) -> Printf.sprintf {|(Sub (%s, %s))|} (show_expr x) (show_expr y)
  | Mul (x, y) -> Printf.sprintf {|(Mul (%s, %s))|} (show_expr x) (show_expr y)
  | Div (x, y) -> Printf.sprintf {|(Div (%s, %s))|} (show_expr x) (show_expr y)
  | Const x -> Printf.sprintf {|(Const %s)|} (show_const x)
  | Var x -> Printf.sprintf {|(Var %s)|} (show_asmvar x)

let show_double_arg = function
  | RegToReg (x, y) ->
      Printf.sprintf {|(RegToReg (%s, %s))|} (show_reg_e x) (show_reg_e y)
  | RegToExpr (x, y) ->
      Printf.sprintf {|(RegToExpr (%s, %s))|} (show_reg x) (show_expr y)
  | RegToVar (x, y) ->
      Printf.sprintf {|(RegToVar (%s, %s))|} (show_reg_e x) (show_asmvar y)

let show_mnemonic : mnemonic -> string = function
  | RET -> "(RET)"
  | PUSH x -> Printf.sprintf {|(PUSH %s)|} (show_reg x)
  | POP x -> Printf.sprintf {|(POP %s)|} (show_reg x)
  | INC x -> Printf.sprintf {|(INC %s)|} (show_reg x)
  | DEC x -> Printf.sprintf {|(DEC %s)|} (show_reg x)
  | NOT x -> Printf.sprintf {|(NOT %s)|} (show_reg x)
  | NEG x -> Printf.sprintf {|(NEG %s)|} (show_reg x)
  | JMP x -> Printf.sprintf {|(JMP %s)|} (show_label x)
  | JE x -> Printf.sprintf {|(JE %s)|} (show_label x)
  | JNE x -> Printf.sprintf {|(JNE %s)|} (show_label x)
  | JZ x -> Printf.sprintf {|(JZ %s)|} (show_label x)
  | JG x -> Printf.sprintf {|(JG %s)|} (show_label x)
  | JGE x -> Printf.sprintf {|(JGE %s)|} (show_label x)
  | JL x -> Printf.sprintf {|(JL %s)|} (show_label x)
  | JLE x -> Printf.sprintf {|(JLE %s)|} (show_label x)
  | MOV x -> Printf.sprintf {|(MOV %s)|} (show_double_arg x)
  | ADD x -> Printf.sprintf {|(ADD %s)|} (show_double_arg x)
  | SUB x -> Printf.sprintf {|(SUB %s)|} (show_double_arg x)
  | IMUL x -> Printf.sprintf {|(IMUL %s)|} (show_double_arg x)
  | AND x -> Printf.sprintf {|(AND %s)|} (show_double_arg x)
  | XOR x -> Printf.sprintf {|(XOR %s)|} (show_double_arg x)
  | OR x -> Printf.sprintf {|(OR %s)|} (show_double_arg x)
  | SHL (x, y) ->
      Printf.sprintf {|(SHL (RegToExpr (%s, %s)))|} (show_reg_e x) (show_expr y)
  | SHR (x, y) ->
      Printf.sprintf {|(SHR (RegToExpr (%s, %s)))|} (show_reg_e x) (show_expr y)
  | CMP x -> Printf.sprintf {|(CMP %s)|} (show_double_arg x)

let show_code_section : code_section -> string = function
  | Command x -> Printf.sprintf {|(Command %s)|} (show_mnemonic x)
  | Id x -> Printf.sprintf {|(Id %s)|} (show_label x)

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
