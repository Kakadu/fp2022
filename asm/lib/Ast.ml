type expr =
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Const of int
  | Reg of string
[@@deriving show { with_path = false }]

type label = Label of string [@@deriving show { with_path = false }]
type mnemonic = Mnemonic of string [@@deriving show { with_path = false }]

type command =
  | Args0 of mnemonic
  | Args1 of mnemonic * expr
  | Args2 of mnemonic * expr * expr
  | Jmp of mnemonic * label
[@@deriving show { with_path = false }]

type code_section =
  | Command of command
  | Id of label
[@@deriving show { with_path = false }]