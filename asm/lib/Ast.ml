type expr =
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Const of string
  | Var of string
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

type data_type = DataType of string [@@deriving show { with_path = false }]

type var = Variable of string * data_type * string list
[@@deriving show { with_path = false }]

type ast =
  | Code of code_section list
  | Data of var list
[@@deriving show { with_path = false }]