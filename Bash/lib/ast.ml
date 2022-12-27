(** Copyright 2021-2022, Chizhov Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string [@@deriving show { with_path = false }]

type var =
  { name : name
  ; subscript : string
  }
[@@deriving show { with_path = false }]

type expr =
  | Plus of expr * expr
  | Minus of expr * expr
  | Slash of expr * expr
  | Asterisk of expr * expr
  | Greater of expr * expr
  | GreaterOrEqual of expr * expr
  | Less of expr * expr
  | LessOrEqual of expr * expr
  | Equal of expr * expr
  | NotEqual of expr * expr
  | Variable of var
  | Number of int
  | Assignment of var * expr
[@@deriving variants, show { with_path = false }]

type substring =
  { name : var
  ; offset : expr
  ; length : expr option
  }
[@@deriving fields, show { with_path = false }]

type from =
  | FromBegin (** [${name#pattern}] | [${name##pattern}] *)
  | FromEnd (** [${name%pattern}] | [${name%%pattern}] *)
[@@deriving variants, show { with_path = false }]

type min_or_max =
  | Min (** [${name#pattern}] | [${name%pattern}] *)
  | Max (** [${name##pattern}] | [${name%%pattern}] *)
[@@deriving variants, show { with_path = false }]

type substr_removal =
  { name : var
  ; pattern : string
  ; from : from
  ; min_or_max : min_or_max
  }
[@@deriving fields, show { with_path = false }]

type substitute_type =
  | One (** [${name/pattern\[/by\]}] *)
  | All (** [${name//pattern\[/by\]}] *)
  | First (** [${name/#pattern\[/by\]}] *)
  | Last (** [${name/%pattern\[/by\]}] *)
[@@deriving variants, show { with_path = false }]

type substitute =
  { name : var
  ; pattern : string
  ; by : string
  ; subst_type : substitute_type
  }
[@@deriving fields, show { with_path = false }]

type ('a, 'b) key_value =
  { key : 'a
  ; value : 'b
  }
[@@deriving fields, show { with_path = false }]

type 'a var_value = (var, 'a) key_value [@@deriving show { with_path = false }]

type param_expansion =
  | PositionalParam of int (** [$digit] | [${int}] *)
  | VarNameExpansion of string (** [$name] *)
  | VarExpansion of var (** [${name /[[subscript]/]} ] *)
  | Length of var (** [${#name}] *)
  | Substring of substring (** [${parameter:offset:length}] *)
  | SubstrRemoval of substr_removal (** see substr_removal (min_or_max, from) *)
  | Substitute of substitute (** see substitute_type *)
[@@deriving show { with_path = false }]

and atom_string =
  | Text of string (** [string] *)
  | ParamExpansion of param_expansion (** see param_expansion *)
  | CommandSubstitution of pipe (** [$(pipe)] | [`pipe`] *)
  | ArithmExpansion of expr (** [$((expr))] *)
[@@deriving show { with_path = false }]

and simple_string =
  | SingleQuotedString of string (** ['string'] *)
  | DoubleQuotedString of atom_string list (** ["atom_string list"] *)
  | AtomString of atom_string list (** [atom_string list] *)
[@@deriving show { with_path = false }]

(*
  single_arg is an array of strings that are not separated by any separators.
  For example, 'word3'word1"word2" is a single_arg, but "word1" word2 not
*)
and single_arg = simple_string list [@@deriving show { with_path = false }]

and arg =
  | SingleArg of single_arg
  | MultipleArgs of single_arg list
      (** After brace expansion, an argument can be expanded to multiple arguments. *)
[@@deriving show { with_path = false }]

(** [{ any_command list; }] *)
and group = any_command list [@@deriving show { with_path = false }]

(* [ [(] single_arg [| single_arg]...) any_command ;;] *)
and case_item = single_arg list * any_command [@@deriving show { with_path = false }]

(** [case by in
     case_item...
     esac] *)
and case_in =
  { by : single_arg
  ; cases : case_item list
  }
[@@deriving show { with_path = false }]

and for_head =
  | WordsIn of string * arg list (** [string in arg list] *)
  | ArithmTriple of expr * expr * expr (** [((expr; expr; expr))] *)
[@@deriving show { with_path = false }]

(** [for head; do commands; done] *)
and for_compound =
  { head : for_head
  ; commands : any_command list
  }
[@@deriving show { with_path = false }]

(* general type for compounds which denotes
   a condition and a sequence of commands *)
and test_then_commands =
  { condition : any_command list
  ; cons_cmds : any_command list
  }
[@@deriving show { with_path = false }]

and loop =
  | WhileCompound of test_then_commands (** while condition; do cons_cmds; done *)
  | UntilCompound of test_then_commands (** until condition; do cons_cmds; done *)
  | ForCompound of for_compound (** see for_compound *)
[@@deriving show { with_path = false }]

and compound =
  | ArithmCompound of expr (** [((expr))]*)
  | Group of group (** see group *)
  | Loop of loop (** see loop *)
  | IfCompound of test_then_commands list
    (*  if test-commands; then
          consequent-commands;
          [elif more-test-commands; then
          more-consequents;]
          [else alternate-consequents;]
        fi *)
  | CaseIn of case_in (** see case_in *)
[@@deriving show { with_path = false }]

(* [var=single_arg] *)
and env_var = single_arg var_value [@@deriving show { with_path = false }]
and 'a varname_assign = (string, 'a) key_value [@@deriving show { with_path = false }]

and var_assign =
  | AtomVariable of env_var (* [var=single_arg] *)
  | IndexedArray of arg list varname_assign (* [varname=(arg [arg...])] *)
  | AssocArray of single_arg varname_assign list varname_assign
    (* [varname=(var=single_arg [[var=single_arg]...] ]))] *)
[@@deriving show { with_path = false }]

and redir =
  | RedirInput of int * single_arg (** [\[fd\]<single_arg] *)
  | RedirOutput of int * single_arg (** [\[fd\]>single_arg] *)
  | AppendOutput of int * single_arg (** [\[fd\]>>single_arg] *)
  | DupInput of int * single_arg (** [\[fd\]<&single_arg] *)
  | DupOutput of int * single_arg (** [\[fd\]>&single_arg] *)
[@@deriving show { with_path = false }]

(* [ [!] [var_assign [var_assign...]] [arg [arg...]] ] *)
(* [ [!] [env_vars]                   [cmd]          ] *)
and atom_operand =
  { invert : bool
  ; env_vars : var_assign list
  ; cmd : arg list
  ; redirs : redir list
  }
[@@deriving fields, show { with_path = false }]

and operand =
  | CompoundOperand of compound
  | AtomOperand of atom_operand (*     [atom_operand] *)
  | OrOperand of operand * operand (*  [atom_operand \|\| atom_operand] *)
  | AndOperand of operand * operand (* [atom_operand && atom_operand] *)
[@@deriving show { with_path = false }]

(* [operand [ [| operand] ]... ] *)
and pipe = Operands of operand list [@@deriving show { with_path = false }]

and any_command =
  | Simple of pipe (* see pipe *)
  | Compound of compound * redir list (* see compound and redir *)
[@@deriving show { with_path = false }]

(* function name () { body } *)
and func =
  { name : name
  ; body : script
  }
[@@deriving show { with_path = false }]

and command_or_func =
  | Command of any_command (* see any_command *)
  | Func of func (* see func *)
[@@deriving show { with_path = false }]

(* [command_or_func  [ [ [\n | ;] command_or_func] ...] *)
and script = command_or_func list [@@deriving show { with_path = false }]

(* ---------------------------------------------------------
   Helper functions that cannot be generated by @@directive
            variants due to mutual type recursion
   --------------------------------------------------------- *)

let positionalparam x = PositionalParam x
let varnameexpansion x = VarNameExpansion x
let varexpansion x = VarExpansion x
let length x = Length x
let substring x = Substring x
let substrremoval x = SubstrRemoval x
let substitute x = Substitute x
let commandsubstitution x = CommandSubstitution x
let arithmexpansion x = ArithmExpansion x
let text x = Text x
let paramexpansion x = ParamExpansion x
let singlequotedstring x = SingleQuotedString x
let doublequotedstring x = DoubleQuotedString x
let atomstring x = AtomString x
let multipleargs x = MultipleArgs x
let singlearg x = SingleArg x
let group x = Group x
let casein x = CaseIn x
let arithmcompound x = ArithmCompound x
let whilecompound x = WhileCompound x
let wordsin x y = WordsIn (x, y)
let arithmtriple x y z = ArithmTriple (x, y, z)
let untilcompound x = UntilCompound x
let forcompound x = ForCompound x
let loop x = Loop x
let ifcompound x = IfCompound x
let atomvariable x = AtomVariable x
let indexedarray x = IndexedArray x
let assocarray x = AssocArray x
let compoundoperand x = CompoundOperand x
let atomoperand x = AtomOperand x
let oroperand x y = OrOperand (x, y)
let andoperand x y = AndOperand (x, y)
let simple x = Simple x
let compound (x, y) = Compound (x, y)
let operands x = Operands x
let redirinput x y = RedirInput (x, y)
let rediroutput x y = RedirOutput (x, y)
let appendoutput x y = AppendOutput (x, y)
let redirinput x y = RedirInput (x, y)
let dupinput x y = DupInput (x, y)
let dupoutput x y = DupOutput (x, y)
let command x = Command x
let func x = Func x
