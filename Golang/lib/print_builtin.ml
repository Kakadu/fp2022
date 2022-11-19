open Base
open Ast
open Ident

let rec pass_expr e =
  let pass_exprs exprs = List.map exprs ~f:pass_expr in
  match e with
  | Call (Ident id, args) ->
    let args = pass_exprs args in
    (match as_builtin id with
     | Some x when String.equal x "print" -> Print args
     | _ -> Call (Ident id, args))
  | ArrLit (t, els) -> ArrLit (t, pass_exprs els)
  | ArrIndex (a, i) -> ArrIndex (pass_expr a, pass_expr i)
  | Call (f, args) -> Call (pass_expr f, pass_exprs args)
  | FuncLit (s, b) -> FuncLit (s, pass_block b)
  | UnOp (op, e) -> UnOp (op, pass_expr e)
  | BinOp (l, op, r) -> BinOp (pass_expr l, op, pass_expr r)
  | e -> e

and pass_stmt = function
  | AssignStmt (l, r) -> AssignStmt (l, pass_expr r)
  | VarDecl (name, r) -> VarDecl (name, pass_expr r)
  | BlockStmt b -> BlockStmt (pass_block b)
  | ExprStmt e -> ExprStmt (pass_expr e)
  | GoStmt e -> GoStmt (pass_expr e)
  | RetStmt e -> RetStmt (Option.map e ~f:pass_expr)
  | IfStmt (cond, b1, b2) -> IfStmt (pass_expr cond, pass_block b1, pass_block b2)

and pass_block stmts = List.map stmts ~f:pass_stmt

let pass_toplevel = function
  | GlobalVarDecl (name, e) -> GlobalVarDecl (name, pass_expr e)
  | FuncDecl (name, s, b) -> FuncDecl (name, s, pass_block b)
;;

let pass_file (f : ident source_file) = List.map f ~f:pass_toplevel
