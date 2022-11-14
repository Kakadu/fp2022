type grammar = string * (string * string list) list

type parseTree =
  | Term of string
  | Nonterm of string * parseTree list