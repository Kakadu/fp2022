(** Copyright 2021-2022, Evgeniy Bakaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom

type help_statements =
  | Expression of expression
  | Assign of expression list * rval
  | Block of int * help_statements
  | MethodDef of identifier * params * help_statements list
  | IfElse of expression * help_statements list * help_statements list
  | Else of help_statements list
  | While of expression * help_statements list
  | For of expression * expression list * help_statements list
  | Class of identifier * help_statements list
  | Return of expression list
  | LvledStmt of int * help_statements
  | ParserError

let parse p s = parse_string ~consume:All p s

let is_whitespace = function
  | ' ' -> true
  | _ -> false
;;

let is_eol = function
  | '\n' -> true
  | _ -> false
;;

let is_eol_or_space = function
  | ' ' | '\n' -> true
  | _ -> false
;;

let tab_as_spaces = "    "
let eolspace = take_while is_eol_or_space
let space = take_while is_whitespace
let space1 = take_while1 is_whitespace
let eol = take_while is_eol
let token s = space *> string s
let comma = eolspace *> token "," <* eolspace
let expr_stmt lvl expr = LvledStmt (lvl, Expression expr)
let arth_add = token "+" *> return (fun e1 e2 -> ArithOp (Add, e1, e2))
let arth_sub = token "-" *> return (fun e1 e2 -> ArithOp (Sub, e1, e2))
let arth_mul = token "*" *> return (fun e1 e2 -> ArithOp (Mul, e1, e2))
let arth_div = token "/" *> return (fun e1 e2 -> ArithOp (Div, e1, e2))
let arth_mod = token "%" *> return (fun e1 e2 -> ArithOp (Mod, e1, e2))
let b_not = token "not" *> return (fun e1 -> UnaryOp (Not, e1))
let b_and = token "and" *> return (fun e1 e2 -> BoolOp (And, e1, e2))
let b_or = token "or" *> return (fun e1 e2 -> BoolOp (Or, e1, e2))
let eq = token "==" *> return (fun e1 e2 -> Eq (e1, e2))
let neq = token "!=" *> return (fun e1 e2 -> NotEq (e1, e2))
let ls = token "<" *> return (fun e1 e2 -> Ls (e1, e2))
let gr = token ">" *> return (fun e1 e2 -> Gr (e1, e2))
let lse = token "<=" *> return (fun e1 e2 -> Lse (e1, e2))
let gre = token ">=" *> return (fun e1 e2 -> Gre (e1, e2))
let high_pr_op = arth_mul <|> arth_div <|> arth_mod <* space
let low_pr_op = space *> arth_add <|> arth_sub <* space
let cmp_op = lse <|> ls <|> gre <|> gr <|> neq <|> eq <* space
let parens p = char '(' *> p <* char ')'

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let is_reserved = function
  | "False"
  | "await"
  | "else"
  | "import"
  | "pass"
  | "None"
  | "break"
  | "except"
  | "in"
  | "raise"
  | "True"
  | "class"
  | "finally"
  | "is"
  | "return"
  | "and"
  | "continue"
  | "for"
  | "lambda"
  | "try"
  | "as"
  | "def"
  | "from"
  | "nonlocal"
  | "while"
  | "assert"
  | "del"
  | "global"
  | "not"
  | "with"
  | "async"
  | "elif"
  | "if"
  | "or"
  | "yield" -> true
  | _ -> false
;;

let%test _ = is_reserved "false" = false
let%test _ = is_reserved "False" = true

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let integer = take_while1 is_digit

let is_valid_first_char = function
  | '_' | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_first_char_capital = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_valid_char = function
  | '_' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false
;;

let identifier =
  space *> peek_char
  >>= function
  | Some c when is_valid_first_char c ->
    take_while is_valid_char
    >>= fun id ->
    (match is_reserved id with
     | false -> return id <* space
     | true -> fail "Reserved identifier")
  | _ -> fail "Invalid first char"
;;

let no_space_identifier =
  peek_char
  >>= function
  | Some c when is_valid_first_char c ->
    take_while is_valid_char
    >>= fun id ->
    (match is_reserved id with
     | false -> return id <* space
     | true -> fail "Reserved identifier")
  | _ -> fail "Invalid first char"
;;

let local_var =
  identifier
  >>= function
  | id -> return (Var (VarName (Local, id)))
;;

let class_field =
  token "self." *> no_space_identifier
  >>= function
  | id -> return (Var (VarName (Class, id)))
;;

let%test _ = parse local_var "a" = Ok (Var (VarName (Local, "a")))

(* still idk will i use these or not *)

(* let global_var = 
  identifier
  >>= function id ->
    return (Var (Global, id))

let class_var = 
  identifier
  >>= function id ->
    return (Var (Class, id)) *)

let sign =
  peek_char
  >>= function
  | Some '-' -> advance 1 >>| fun () -> "-"
  | Some '+' -> advance 1 >>| fun () -> "+"
  | Some c when is_digit c -> return "+"
  | _ -> fail "Sign or digit expected"
;;

let dot =
  peek_char
  >>= function
  | Some '.' -> advance 1 >>| fun () -> true
  | _ -> return false
;;

let number =
  sign
  >>= fun sign ->
  take_while1 is_digit
  >>= fun whole ->
  dot
  >>= function
  | false -> return (Const (Integer (int_of_string (sign ^ whole))))
  | true ->
    take_while1 is_digit
    >>= fun part ->
    return (Const (Float (float_of_string (Printf.sprintf "%s%s.%s" sign whole part))))
;;

let%test _ = parse number "-1.23" = Ok (Const (Float (-1.23)))

let p_string =
  char '\"'
  *> take_while1 (function
       | '\"' -> false
       | _ -> true)
  <* char '\"'
  >>= fun s -> return (Const (String s))
;;

let%test _ = parse p_string "\"1.23\"" = Ok (Const (String "1.23"))

let p_params = sep_by comma identifier

let%test _ = parse p_params "  a  ,   b,c    " = Ok [ "a"; "b"; "c" ]

(* python possible code:
                someList = [1, 2,
                              
                              3,
                                 4] *)
let lst p =
  token "[" *> sep_by (token "," *> space *> take_while is_eol) p
  <* token "]"
  >>= fun items -> return (List items)
;;

let parse_class tabs =
  token "class" *> identifier
  <* token ":"
  >>= fun id -> return (Block (tabs, Class (id, [])))
;;

(* someMethod(someParams) *)
let method_call p =
  identifier
  >>= fun idd ->
  token "(" *> sep_by comma p <* token ")" >>= fun args -> return (MethodCall (idd, args))
;;

let class_instance p =
  match peek_char_fail with
  | c ->
    c
    >>= fun chr ->
    (match is_first_char_capital chr with
     | true ->
       identifier
       >>= fun id ->
       token "(" *> sep_by comma p
       <* token ")"
       >>= fun args -> return (ClassToInstance (id, args))
     | false -> fail "SAD")
;;

(* someClass.someField *)
let access_field =
  identifier
  >>= fun id -> token "." *> identifier >>= fun field -> return (FieldAccess (id, field))
;;

let parse_return expr tabs =
  token "return" *> sep_by comma expr >>= fun ret -> return (LvledStmt (tabs, Return ret))
;;

(* someClass.someMethod(someParams) *)
let access_method p =
  identifier
  >>= fun id ->
  token "." *> identifier
  >>= fun mthd ->
  token "(" *> sep_by comma p
  <* token ")"
  >>= fun args -> return (MethodAccess (id, mthd, args))
;;

(* lambda x1, .., xn:  stmts*)
let lambda expr =
  token "lambda" *> space1 *> p_params
  >>= fun args -> token ":" *> expr >>= fun stmts -> return (Lambda (args, stmts))
;;

let parse_def tabs =
  token "def" *> identifier
  >>= fun id ->
  token "(" *> p_params
  >>= fun args ->
  token ")" *> token ":" >>= fun _ -> return (Block (tabs, MethodDef (id, args, [])))
;;

let assign expr tabs =
  sep_by comma (class_field <|> access_field <|> local_var)
  >>= fun lvalues ->
  token "=" *> space *> sep_by comma expr
  >>= fun rvalues -> return (LvledStmt (tabs, Assign (lvalues, rvalues)))
;;

let parse_if expr tabs =
  token "if" *> space1 *> expr
  <* token ":"
  >>= fun if_expr -> return (Block (tabs, IfElse (if_expr, [], [])))
;;

let parse_else tabs =
  token "else" *> token ":" >>= fun _ -> return (Block (tabs, Else []))
;;

let parse_while expr tabs =
  token "while" *> expr
  <* token ":"
  >>= fun while_expr -> return (Block (tabs, While (while_expr, [])))
;;

let parse_for expr tabs =
  token "for" *> local_var
  >>= fun id ->
  token "in" *> token "range" *> token "(" *> sep_by comma expr
  <* token ")"
  <* token ":"
  >>= fun range -> return (Block (tabs, For (id, range, [])))
;;

(* maybe if and else should be merged *)
let take_block_from_stmt = function
  | MethodDef (_, _, stmts) -> stmts
  | For (_, _, stmts) -> stmts
  | IfElse (_, stmts, _) -> stmts
  | Else stmts -> stmts
  | While (_, stmts) -> stmts
  | Class (_, stmts) -> stmts
  | _ -> []
;;

let insert_to_stmt new_stmts = function
  | MethodDef (i, p, _) -> MethodDef (i, p, new_stmts)
  | For (e1, e2, _) -> For (e1, e2, new_stmts)
  | Else _ -> Else new_stmts
  | IfElse (e, _, else_stmts) -> IfElse (e, new_stmts, else_stmts)
  | While (e, _) -> While (e, new_stmts)
  | Class (e, _) -> Class (e, new_stmts)
  | _ -> ParserError
;;

let rec help_to_ast = function
  | MethodDef (i, p, stmts) ->
    Ast.MethodDef (i, p, List.map (fun x -> help_to_ast x) stmts)
  | For (e1, e2, stmts) -> For (e1, e2, List.map (fun x -> help_to_ast x) stmts)
  | IfElse (e, stmts, else_stmts) ->
    IfElse
      ( e
      , List.map (fun x -> help_to_ast x) stmts
      , List.map (fun x -> help_to_ast x) else_stmts )
  | While (e, stmts) -> While (e, List.map (fun x -> help_to_ast x) stmts)
  | Class (e, stmts) -> Class (e, List.map (fun x -> help_to_ast x) stmts)
  | Block (_, stmt) -> help_to_ast stmt
  | LvledStmt (_, stmt) -> help_to_ast stmt
  | Return x -> Return x
  | Assign (l, r) -> Assign (l, r)
  | Expression e -> Expression e
  | _ -> failwith "parser failed"
;;

let check_is_first_block_empty block =
  let rec check = function
    | [] -> true
    | Block (_, stmt) :: _ ->
      (match take_block_from_stmt stmt with
       | [] -> true
       | _ -> check (take_block_from_stmt stmt))
    | _ -> false
  in
  check block
;;

let%test _ =
  check_is_first_block_empty
    [ Block
        ( 0
        , IfElse
            ( Const (Integer 0)
            , [ Block
                  ( 1
                  , IfElse
                      ( Const (Integer 0)
                      , [ LvledStmt (4, Expression (Const (Integer 5))) ]
                      , [] ) )
              ]
            , [] ) )
    ]
  = false
;;

let%test _ =
  check_is_first_block_empty
    [ Block (0, IfElse (Const (Integer 0), [], []))
    ; Block
        ( 0
        , IfElse
            ( Const (Integer 0)
            , [ Block
                  ( 1
                  , IfElse
                      ( Const (Integer 0)
                      , [ Block (1, IfElse (Const (Integer 0), [], [])) ]
                      , [] ) )
              ]
            , [] ) )
    ]
  = true
;;

let merge_if_else else_stmts = function
  | IfElse (e, s, s2) ->
    (match s2 with
     | [] -> return (IfElse (e, s, else_stmts))
     | _ -> fail "parse else error")
  | _ -> fail "unexpected stmt"
;;

let remove_lvling list =
  let rec rvrs acc = function
    | [] -> return acc
    | Block (_, stmt) :: t ->
      (match stmt with
       | IfElse (_, _, _) as iff ->
         (match acc with
          | [] ->
            rvrs [] (take_block_from_stmt stmt)
            >>= fun rstmt -> rvrs (insert_to_stmt rstmt stmt :: acc) t
          | h :: t1 ->
            (match h with
             | Else s -> merge_if_else s iff >>= fun ifelse -> rvrs (ifelse :: t1) t
             | _ ->
               rvrs [] (take_block_from_stmt stmt)
               >>= fun rstmt -> rvrs (insert_to_stmt rstmt stmt :: acc) t))
       | _ ->
         rvrs [] (take_block_from_stmt stmt)
         >>= fun rstmt -> rvrs (insert_to_stmt rstmt stmt :: acc) t)
    | LvledStmt (_, stmt) :: t -> rvrs (stmt :: acc) t
    | _ -> return []
  in
  rvrs [] list >>= fun res -> return (List.map (fun x -> help_to_ast x) res)
;;

let flatten list =
  let rec insert_ acc_lines = function
    | [] -> return acc_lines
    | Block (n, stmt) :: t ->
      (match acc_lines with
       | [] -> insert_ (Block (n, stmt) :: acc_lines) t
       | x :: t1 ->
         (match x with
          | Block (m, stmt1) ->
            if m > n
            then (
              match check_is_first_block_empty (take_block_from_stmt stmt1) with
              | true -> fail "check tabs. block is empty"
              | _ -> insert_ (Block (n, stmt) :: acc_lines) t)
            else if m = n
            then (
              match check_is_first_block_empty (take_block_from_stmt stmt1) with
              | true -> fail "check tabs. block is empty"
              | _ -> insert_ (Block (n, stmt) :: acc_lines) t)
            else (
              let subblock = insert_ (take_block_from_stmt stmt1) [ Block (n, stmt) ] in
              subblock >>= fun sb -> insert_ (Block (m, insert_to_stmt sb stmt1) :: t1) t)
          | LvledStmt (m, _) ->
            if m < n
            then fail "parser error"
            else insert_ (Block (n, stmt) :: acc_lines) t
          | _ -> fail "unreachable"))
    | LvledStmt (n, stmt) :: t ->
      (match acc_lines with
       | [] -> insert_ (LvledStmt (n, stmt) :: acc_lines) t
       | x :: t1 ->
         (match x with
          | Block (m, stmt1) ->
            if m >= n
            then (
              match check_is_first_block_empty (take_block_from_stmt stmt1) with
              | true -> fail "check tabs. block is empty"
              | _ -> insert_ (LvledStmt (n, stmt) :: acc_lines) t)
            else (
              let subblock =
                insert_ (take_block_from_stmt stmt1) [ LvledStmt (n, stmt) ]
              in
              subblock >>= fun sb -> insert_ (Block (m, insert_to_stmt sb stmt1) :: t1) t)
          | LvledStmt (m, _) ->
            if m = n
            then insert_ (LvledStmt (n, stmt) :: acc_lines) t
            else fail "parser error"
          | _ -> fail "unreachable"))
    | _ -> fail "unreachable"
  in
  insert_ [] list
;;

let prog =
  let expr =
    fix (fun expr ->
      let predict =
        eolspace *> peek_char_fail
        >>= function
        | x when is_valid_first_char x ->
          class_field
          <|> class_instance expr
          <|> access_method expr
          <|> access_field
          <|> method_call expr
          <|> local_var
          <|> lambda expr
        | x when is_digit x -> number
        | '(' -> parens expr
        | '[' -> lst expr
        | '\"' -> p_string
        | _ -> fail "not implemented yet"
      in
      let high = chainl1 predict high_pr_op in
      let low = chainl1 high low_pr_op in
      let cmp = chainl1 low cmp_op in
      let bfactor =
        fix (fun bfactor ->
          let nnot = b_not <* space <*> bfactor in
          choice [ nnot; cmp ])
      in
      let bterm = chainl1 bfactor (b_and <* space) in
      chainl1 bterm (b_or <* space))
  in
  let stmt =
    fix (fun _ ->
      let pexpr =
        many (string "\t" <|> string tab_as_spaces)
        >>= fun tabs -> expr >>| expr_stmt (List.length tabs)
      in
      let passign =
        many (token "\t" <|> string tab_as_spaces)
        >>= fun tabs -> assign expr (List.length tabs)
      in
      let predict =
        many (string "\t" <|> string tab_as_spaces)
        >>= fun tabs ->
        let lvl = List.length tabs in
        peek_char_fail
        >>= function
        | 'i' -> parse_if expr lvl
        | 'e' -> parse_else lvl
        | 'w' -> parse_while expr lvl
        | 'f' -> parse_for expr lvl
        | 'd' -> parse_def lvl
        | 'r' -> parse_return expr lvl
        | 'c' -> parse_class lvl
        | _ -> expr >>| expr_stmt lvl
      in
      choice [ passign; predict; pexpr ])
  in
  take_while (fun c -> is_eol c) *> sep_by (token "\n") stmt
  <* take_while (fun c -> is_eol c)
  >>= fun lines ->
  flatten lines
  >>= fun res ->
  if check_is_first_block_empty res then fail "parser error" else remove_lvling res
;;

let%test _ = parse prog "5" = Ok [ Expression (Const (Integer 5)) ]

let%test _ =
  parse prog "\nx,\ny = 1, 2"
  = Ok
      [ Assign
          ( [ Var (VarName (Local, "x")); Var (VarName (Local, "y")) ]
          , [ Const (Integer 1); Const (Integer 2) ] )
      ]
;;

let%test _ =
  parse prog "\ti > 4"
  = Ok [ Expression (Gr (Var (VarName (Local, "i")), Const (Integer 4))) ]
;;

let%test _ =
  parse prog "5 and\n 4"
  = Ok [ Expression (BoolOp (And, Const (Integer 5), Const (Integer 4))) ]
;;

let%test _ =
  parse prog "\t[5+3, 4+6]"
  = Ok
      [ Expression
          (List
             [ ArithOp (Add, Const (Integer 5), Const (Integer 3))
             ; ArithOp (Add, Const (Integer 4), Const (Integer 6))
             ])
      ]
;;

let%test _ =
  parse prog "[f(5)+f(3), \ncat.head,\n\n           cat.set(10, 20+10)]"
  = Ok
      [ Expression
          (List
             [ ArithOp
                 ( Add
                 , MethodCall ("f", [ Const (Integer 5) ])
                 , MethodCall ("f", [ Const (Integer 3) ]) )
             ; FieldAccess ("cat", "head")
             ; MethodAccess
                 ( "cat"
                 , "set"
                 , [ Const (Integer 10)
                   ; ArithOp (Add, Const (Integer 20), Const (Integer 10))
                   ] )
             ])
      ]
;;

let%test _ =
  parse prog "[f(5)+\nf(\n3), \ncat.head,\n\n           cat.set(\n10\n,\n 20+10)]"
  = Ok
      [ Expression
          (List
             [ ArithOp
                 ( Add
                 , MethodCall ("f", [ Const (Integer 5) ])
                 , MethodCall ("f", [ Const (Integer 3) ]) )
             ; FieldAccess ("cat", "head")
             ; MethodAccess
                 ( "cat"
                 , "set"
                 , [ Const (Integer 10)
                   ; ArithOp (Add, Const (Integer 20), Const (Integer 10))
                   ] )
             ])
      ]
;;

let%test _ =
  parse prog "if 0:\n\t4"
  = Ok [ IfElse (Const (Integer 0), [ Expression (Const (Integer 4)) ], []) ]
;;

let%test _ =
  parse prog "if 0:\n\t1\n\tif 1:\n\t\t2\n\t1\n\t1"
  = Ok
      [ IfElse
          ( Const (Integer 0)
          , [ Expression (Const (Integer 1))
            ; IfElse (Const (Integer 1), [ Expression (Const (Integer 2)) ], [])
            ; Expression (Const (Integer 1))
            ; Expression (Const (Integer 1))
            ]
          , [] )
      ]
;;

let%test _ =
  parse prog "if 0:\n\t0\nif 1:\n\t1"
  = Ok
      [ IfElse (Const (Integer 0), [ Expression (Const (Integer 0)) ], [])
      ; IfElse (Const (Integer 1), [ Expression (Const (Integer 1)) ], [])
      ]
;;

let%test _ =
  parse prog "if \"a\":\n\t0"
  = Ok [ IfElse (Const (String "a"), [ Expression (Const (Integer 0)) ], []) ]
;;

let%test _ =
  parse prog "while not x > 0:\n\tx = x -1\n\tcat.head"
  = Ok
      [ While
          ( UnaryOp (Not, Gr (Var (VarName (Local, "x")), Const (Integer 0)))
          , [ Assign
                ( [ Var (VarName (Local, "x")) ]
                , [ ArithOp (Sub, Var (VarName (Local, "x")), Const (Integer 1)) ] )
            ; Expression (FieldAccess ("cat", "head"))
            ] )
      ]
;;

let%test _ =
  parse prog "lambda x,y: x+y"
  = Ok
      [ Expression
          (Lambda
             ( [ "x"; "y" ]
             , ArithOp (Add, Var (VarName (Local, "x")), Var (VarName (Local, "y"))) ))
      ]
;;

let%test _ =
  parse prog "def sum(x,y):\n\treturn x+y"
  = Ok
      [ MethodDef
          ( "sum"
          , [ "x"; "y" ]
          , [ Return
                [ ArithOp (Add, Var (VarName (Local, "x")), Var (VarName (Local, "y"))) ]
            ] )
      ]
;;

let%test _ =
  parse prog "for i in range(1, 10):\n    1"
  = Ok
      [ For
          ( Var (VarName (Local, "i"))
          , [ Const (Integer 1); Const (Integer 10) ]
          , [ Expression (Const (Integer 1)) ] )
      ]
;;

(* im not sure that its fine *)
let%test _ =
  parse prog "a.b = x"
  = Ok [ Assign ([ FieldAccess ("a", "b") ], [ Var (VarName (Local, "x")) ]) ]
;;

let%test _ =
  parse prog "class A:\n\tdef test(self, x):\n\t\tself.x = x\n\t\treturn x"
  = Ok
      [ Class
          ( "A"
          , [ MethodDef
                ( "test"
                , [ "self"; "x" ]
                , [ Assign ([ Var (VarName (Class, "x")) ], [ Var (VarName (Local, "x")) ])
                  ; Return [ Var (VarName (Local, "x")) ]
                  ] )
            ] )
      ]
;;

let%test _ =
  parse prog "a = A()"
  = Ok [ Assign ([ Var (VarName (Local, "a")) ], [ ClassToInstance ("A", []) ]) ]
;;

let%test _ =
  parse prog "self.x = x"
  = Ok [ Assign ([ Var (VarName (Class, "x")) ], [ Var (VarName (Local, "x")) ]) ]
;;

(* "def fact(n):\n\
     \tif n < 0:\n\
     \t\treturn\n\
     \tif n == 0 or n == 1:\n\
     \t\treturn 1\n\
     \treturn n * fact(n-1)" *)
(*{|
    def fact(n):
      if n < 0 :
        return n
      if n == 0 or n == 1:
        return 1
      return n * fact(n-1)
    |}  *)
let%test _ =
  parse
    prog
    "def fact(n):\n\
     \tif n < 0:\n\
     \t\treturn\n\
     \tif n == 0 or n == 1:\n\
     \t\treturn 1\n\
     \treturn n * fact(n-1)"
  = Ok
      [ MethodDef
          ( "fact"
          , [ "n" ]
          , [ IfElse
                (Ls (Var (VarName (Local, "n")), Const (Integer 0)), [ Return [] ], [])
            ; IfElse
                ( BoolOp
                    ( Or
                    , Eq (Var (VarName (Local, "n")), Const (Integer 0))
                    , Eq (Var (VarName (Local, "n")), Const (Integer 1)) )
                , [ Return [ Const (Integer 1) ] ]
                , [] )
            ; Return
                [ ArithOp
                    ( Mul
                    , Var (VarName (Local, "n"))
                    , MethodCall
                        ( "fact"
                        , [ ArithOp (Sub, Var (VarName (Local, "n")), Const (Integer 1)) ]
                        ) )
                ]
            ] )
      ]
;;

let%test _ =
  parse
    prog
    "class Node:\n\
     \tdef init(v):\n\
     \t\tself.value = v\n\
     \tdef get():\n\
     \t\treturn self.value\n\
     node1 = Node()\n\
     node1.init(5)\n\
     node2 = Node()\n\
     node2.init(10)\n\
     [node1.get(), node2.get()]"
  = Ok
      [ Class
          ( "Node"
          , [ MethodDef
                ( "init"
                , [ "v" ]
                , [ Assign
                      ([ Var (VarName (Class, "value")) ], [ Var (VarName (Local, "v")) ])
                  ] )
            ; MethodDef ("get", [], [ Return [ Var (VarName (Class, "value")) ] ])
            ] )
      ; Assign ([ Var (VarName (Local, "node1")) ], [ ClassToInstance ("Node", []) ])
      ; Expression (MethodAccess ("node1", "init", [ Const (Integer 5) ]))
      ; Assign ([ Var (VarName (Local, "node2")) ], [ ClassToInstance ("Node", []) ])
      ; Expression (MethodAccess ("node2", "init", [ Const (Integer 10) ]))
      ; Expression
          (List [ MethodAccess ("node1", "get", []); MethodAccess ("node2", "get", []) ])
      ]
;;

let%test _ =
  parse prog "if 5:\n\t5\nif 6:\n\t6\nelse:\n\t0\n\t1"
  = Ok
      [ IfElse (Const (Integer 5), [ Expression (Const (Integer 5)) ], [])
      ; IfElse
          ( Const (Integer 6)
          , [ Expression (Const (Integer 6)) ]
          , [ Expression (Const (Integer 0)); Expression (Const (Integer 1)) ] )
      ]
;;
