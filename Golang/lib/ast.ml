(* Types *)

type typ =
  | IntTyp
  | ArrayTyp of array_typ
  | FunTyp of string signature
  | StrTyp
  | BoolTyp
[@@deriving show, ord]

and array_typ = int * typ

and 'id signature =
  { args : 'id arg list
  ; ret : return_typ
  }

and 'id arg = 'id * typ

and return_typ =
  | One of typ
  | Void

(* Operators *)

type unaryop =
  | Minus
  | Not
[@@deriving show]

type binop =
  | Mul
  | Div
  | Mod
  | Add
  | Sub
  | Eq
  | Neq
  | Lt
  | Lte
  | Gt
  | Gte
  | And
  | Or
[@@deriving show]

(* Literals *)
type constant =
  | Int of int
  | Str of string
  | Bool of bool
[@@deriving show]

(* Nodes *)

type 'id expr =
  | Const of constant
  | Ident of 'id
  | ArrLit of (array_typ * 'id expr list)
  | ArrIndex of ('id expr * 'id expr)
  | Call of ('id expr * 'id expr list)
  | FuncLit of ('id signature * 'id block)
  | UnOp of (unaryop * 'id expr)
  | BinOp of ('id expr * binop * 'id expr)
  | Print of 'id expr list
[@@deriving show]

and 'id var_decl = 'id * 'id expr
and 'id func_decl = 'id * 'id signature * 'id block

and 'id top_level_decl =
  | GlobalVarDecl of 'id var_decl
  | FuncDecl of 'id func_decl
[@@deriving show]

and 'id block = 'id stmt list [@@deriving show, ord]

and 'id stmt =
  | AssignStmt of 'id expr * 'id expr
  | VarDecl of 'id var_decl
  | BlockStmt of 'id block
  | ExprStmt of 'id expr
  | GoStmt of 'id expr
  | RetStmt of 'id expr option
  | IfStmt of 'id expr * 'id block * 'id block
[@@deriving show, ord]

type 'id source_file = 'id top_level_decl list [@@deriving show]

let rec show_typ = function
  | IntTyp -> "int"
  | StrTyp -> "string"
  | BoolTyp -> "bool"
  | ArrayTyp (len, eltyp) -> Printf.sprintf "[%d]%s" len (show_typ eltyp)
  | FunTyp { args; ret } ->
    let args = List.map (fun (_, typ) -> show_typ typ) args in
    let ret =
      match ret with
      | Void -> ""
      | One t -> " " ^ show_typ t
    in
    Printf.sprintf "fn(%s)%s" (String.concat ", " args) ret
;;

let eq_typ t1 t2 = compare_typ t1 t2 = 0
