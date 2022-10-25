type _type =
  | TypeInt
  | TypeVoid
  | String (* TODO: Done with stdlib type, remake it *)
  | TypeBool
  | TypeClass of string
  | Array of _type * int
[@@deriving show { with_path = false }]

type modifier = Public | Static | Const | Async
[@@deriving show { with_path = false }]

type values =
  | ValInt of int
  | ValString of string (* TODO: string? *)
  | ValNull
  | ValVoid
  | ValBool of bool
  | ValClass of obj_ref
[@@deriving show { with_path = false }]

and obj_ref =
  | ObjNull
  | ObjRef of string * string option (* TODO: class key and parent key??*)
[@@deriving show { with_path = false }]

type expr =
  | Value of values
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  | PreInc of expr
  | PostInc of expr
  | PreDec of expr
  | PostDec of expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Equal of expr * expr
  | NonEqual of expr * expr
  | Less of expr * expr
  | More of expr * expr
  | LessEqual of expr * expr
  | MoreEqual of expr * expr
  | Null
  | Var of string
  | ConstExpr of values
  | Assign of expr * expr (* = *)
  | ArrayAccess of expr * expr
  | FuncCall of string * expr list
  | Lambda of expr (* ??? *)
  | Await of expr (* ?? *)
  | Linq of expr (* ??? *)
[@@deriving show { with_path = false }]

and stmt =
  | For of
      stmt option
      * expr option
      * expr list
      * stmt (*because you can write for(int i = 0, j = 5; i < 4; i++, j--)*)
  | If of expr * stmt * stmt option (*statement option is "else"*)
  | While of expr * stmt
  | Return of expr option
  | StmtsBlock of stmt list
  | Break
  | Continue
  | VarDeclr of modifier option * _type * (string * expr option) list
  | Expr of expr
  | Print of expr
[@@deriving show { with_path = false }]

and field =
  | VarField of _type * (string * expr option) list
  | Method of
      modifier list (* TODO: option? *)
      * _type
      * string
      * (_type * string) list
      * stmt
[@@deriving show { with_path = false }]

and c_sharp_class =
  | Class of
      modifier list
      * string (* class name*)
      * string option (* TODO: remove parent class name*)
      * (modifier list * field) list (* Methods *)
[@@deriving show { with_path = false }]
