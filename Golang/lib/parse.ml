(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

(* Utils *)

let string_of_char c = String.make 1 c
let parse p s = parse_string ~consume:All p s

(* ============================================================
   ---------------      Primitive parsers       ---------------
   ============================================================ *)

(* ===== Predicates ===== *)

let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

(* ===== Token parsers ===== *)

let integer =
  let* s = take_while1 is_digit in
  return (int_of_string s)
;;

let ws = take_while is_whitespace
let ws1 = take_while1 is_whitespace
let semi = char ';'
let keyword kw = string kw

let ident_str =
  let is_ident_start = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | _ -> false
  in
  let is_ident_mid = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  in
  let* first = satisfy is_ident_start in
  let* rest = take_while is_ident_mid in
  return (string_of_char first ^ rest)
;;

(* ===== Helper parsers ===== *)

let in_parens p = char '(' *> ws *> p <* ws <* char ')'
let in_brackets p = char '[' *> ws *> p <* ws <* char ']'
let in_braces p = char '{' *> ws *> p <* ws <* char '}'

(** If the next char is [c] consumes it. Never fails *)
let eat c =
  let* _ = option ' ' (char c) in
  return ()
;;

(** Parses comma-separated [p]s allowing trailing commas *)
let comma_separated p =
  let* ps = sep_by (char ',' <* ws) (p <* ws) in
  let* _ = eat ',' in
  return ps
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

(* ============================================================
   ---------  String content & escape sequences ---------------
   ============================================================ *)

(* Here we parse bodies of string literals (exluding the quotes).

   StringBody := StrChar*.
   StrChar := any char including escape sequences, excluding newlines and unescaped double quotes.

   Every escape sequence contains two parts:  a '\' prefix and a payload that encodes the value.
*)

let payload_simple =
  any_char
  >>= function
  | 'a' -> return "\u{0007}"
  | 'b' -> return "\u{0008}"
  | 'f' -> return "\u{000C}"
  | 'n' -> return "\u{000A}"
  | 'r' -> return "\u{000D}"
  | 't' -> return "\u{0009}"
  | 'v' -> return "\u{000B}"
  | '\\' -> return "\u{005C}"
  | '"' -> return "\u{0022}"
  | _ -> fail "Illegal escaped character"
;;

let payload_octal = take 3 >>= fun _ -> fail "octal not implemented"
let payload_hex = char 'x' *> take 2 >>= fun _ -> fail "hex not implemented"
let payload_little_u = char 'u' *> take 4 >>= fun _ -> fail "little u not implemented"
let payload_big_u = char 'U' *> take 8 >>= fun _ -> fail "big U not implemented"

let escape_seq =
  let* _ = char '\\' in
  let* c = peek_char_fail in
  match c with
  | '0' .. '7' -> payload_octal
  | 'x' -> payload_hex
  | 'u' -> payload_little_u
  | 'U' -> payload_big_u
  | _ -> payload_simple
;;

let str_char =
  let* c = peek_char_fail in
  match c with
  | '\\' ->
    let* decoded_c = escape_seq in
    return decoded_c
  | '\n' | '"' -> fail "Invalid character in string"
  | _ ->
    let* c = any_char in
    return (string_of_char c)
;;

(* Parses the body of a string literal *)
let string_body =
  let* parts = many str_char in
  return (List.fold_left ( ^ ) "" parts)
;;

(* ============================================================
   ---------        Constants & Identifiers     ---------------
   ============================================================ *)

let ident = ident_str >>= fun i -> return (Ident i)

let int_const =
  let* i = integer in
  return (Int i)
;;

let str_const =
  (let* content = char '"' *> string_body <* char '"' in
   return (Str content))
  <?> "StringConstant"
;;

(* ============================================================
   ---------                  Types             ---------------
   ============================================================ *)

(* Type := TypeName | TypeLit | "(" Type ")".
   TypeLit := ArrayType | FunctionType. *)

type type_dispatch =
  { signature : type_dispatch -> string signature t
  ; array_typ : type_dispatch -> array_typ t
  ; typ : type_dispatch -> typ t
  }

let type_dis =
  (* ==================== *)
  (* ===== TypeName ===== *)
  let type_name =
    let* name = ident_str in
    match name with
    | "int" -> return IntTyp
    | "string" -> return StrTyp
    | "bool" -> return BoolTyp
    | _ -> fail "Unknown type"
  in
  (* ===================== *)
  (* ===== ArrayType ===== *)

  (* ArrayType := "[" ArrayLength "]" ElementType.
      ArrayLength := integer constant.
      ElementType := Type. *)
  let array_type d =
    let* _ = in_brackets ws <* ws in
    let* el_type = d.typ d in
    return { el = el_type }
  in
  let array_type_wrapped d =
    let* t = array_type d in
    return (ArrayTyp t)
  in
  (* ====================== *)
  (* ===== Signatures ===== *)

  (* Signature := Parameters Result? .
     Result := Type .
     Parameters := "(" ParameterList? ")" .
     ParameterList := ParameterDecl ( "," ParameterDecl )* .
     ParameterDecl := Ident Type . *)
  (* TODO: Simplified *)
  let parameter_decl d =
    let* name = ident_str <* ws1 in
    let* t = d.typ d in
    return (name, t)
  in
  let parameter_list d = comma_separated (parameter_decl d) in
  let parameters d = in_parens (parameter_list d) <?> "Parameters" in
  let result d =
    (let* t = d.typ d in
     return (One t))
    <?> "Result"
  in
  let optional_result d = option Void (result d) in
  let signature d =
    (let* args = parameters d in
     let* ret = ws *> optional_result d in
     return { args; ret })
    <?> "Signature"
  in
  (* ======================= *)
  (* =====  All Types  ===== *)

  (* FuntionType := "func" Signature. *)
  let function_type d =
    let* sign = keyword "func" *> ws *> signature d in
    return (FunTyp sign)
  in
  let chan_type d =
    let* el = string "chan" *> ws1 *> d.typ d in
    return (ChanTyp el)
  in
  let type_lit d =
    function_type d <|> array_type_wrapped d <|> chan_type d <?> "TypeLit"
  in
  let typ d = fix @@ fun typ -> type_name <|> type_lit d <|> in_brackets typ <?> "Type" in
  { signature; typ; array_typ = array_type }
;;

(** Parses type literals  *)
let typ = type_dis.typ type_dis

(** Parses function signatures  *)
let signature = type_dis.signature type_dis

(** Parses array types  *)
let array_typ = type_dis.array_typ type_dis

(* ============================================================
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ============================================================ *)

type eds_dispatch =
  { expr : eds_dispatch -> string expr t
  ; expr_list : eds_dispatch -> string expr list t
  ; (* Declarations *)
    var_decl : eds_dispatch -> string var_decl t
  ; func_decl : eds_dispatch -> string func_decl t
  ; top_level_decl : eds_dispatch -> string top_level_decl t
  ; (* Statements *)
    block : eds_dispatch -> string block t
  ; stmt : eds_dispatch -> string stmt t
  }

(* A helper type that represents a postfix operation of indexing an array `x[i]` or calling a function `x(1, 2, 3)` *)
type args_or_index =
  | Args of string expr list
  | Index of string expr

let eds =
  (* ============================================================
     --------               Expressions                  --------
     ============================================================ *)

  (* Expression := PrimaryExpression.
     TODO: add operators *)

  (* === Operand ===

     Operand := Literal | Ident | "(" Expression ")".
     Literal := StrConstant | IntConstant | ArrayLit | FunctionLit.
     ArrayLit := ArrayType "{" ExprList "}".
  *)
  let array_lit d =
    (let* t = array_typ <* ws in
     let* elements = in_braces (d.expr_list d) in
     return (ArrLit (t, elements)))
    <?> "ArrayLit"
  in
  let str_lit =
    let* s = str_const in
    return (Const s)
  in
  let int_lit =
    let* i = int_const in
    return (Const i)
  in
  let func_lit d =
    (let* _ = keyword "func" *> ws in
     let* sign = signature <* ws in
     let* body = d.block d in
     return (FuncLit (sign, body)))
    <?> "FunctionLit"
  in
  let literal d = str_lit <|> int_lit <|> array_lit d <|> func_lit d <?> "Literal" in
  (* literal and ident have common prefixes: func_lit and ident both start with a word *)
  let operand d = in_parens (d.expr d) <|> literal d <|> ident <?> "Operand" in
  (* === Primary Expression ===

     PrimaryExpr :=
       Operand
       | PrimaryExpr Index
       | PrimaryExpr Arguments .
     Index := "[" Expression "]" .
     Arguments := "(" ExprList? ")". *)
  let index d =
    (let* i = in_brackets (d.expr d) in
     return (Index i))
    <?> "Index"
  in
  let arguments d =
    (let* args = in_parens (d.expr_list d) in
     return (Args args))
    <?> "Arguments"
  in
  let apply_index_or_args receiver = function
    | Index i -> ArrIndex (receiver, i)
    | Args args -> Call (receiver, args)
  in
  let primary_expr d =
    (let* op = operand d <* ws in
     let* operations = many (index d <|> arguments d <* ws) in
     let res = List.fold_left apply_index_or_args op operations in
     return res)
    <?> "PrimaryExpr"
  in
  (* === Unary operators  === *)
  let unary_op op s =
    let* _ = ws *> string s <* ws in
    return (fun x -> UnOp (op, x))
  in
  let unary_ops = unary_op Not "!" <|> unary_op Minus "-" <|> unary_op Receive "<-" in
  let unary_expr d =
    (fix
    @@ fun unary_expr ->
    let unary_op_expr =
      let* op = ws *> unary_ops <* ws in
      let* expr = unary_expr in
      return (op expr)
    in
    unary_op_expr <|> primary_expr d)
    <?> "UnaryExpr"
  in
  (* === Binary operators === *)
  let binop_chainl atom conf =
    let binop op s =
      let* _ = ws *> string s <* ws in
      let make_ast x y = BinOp (x, op, y) in
      return make_ast
    in
    let fold_binops acc x =
      let op, s = x in
      acc <|> binop op s
    in
    match conf with
    | first :: rest ->
      let op, s = first in
      let chainer = List.fold_left fold_binops (binop op s) rest in
      chainl1 atom chainer
    | _ -> atom
  in
  let binops d confs = List.fold_left binop_chainl (unary_expr d) confs in
  (* From higher precedence to lower *)
  let binop_expr d =
    binops
      d
      [ [ Mul, "*"; Div, "/"; Mod, "%" ]
      ; [ Add, "+"; Sub, "-" ]
      ; [ Eq, "=="; Neq, "!="; Lt, "<"; Lte, "<="; Gt, ">"; Gte, ">=" ]
      ; [ And, "&&" ]
      ; [ Or, "||" ]
      ]
  in
  let expr d = fix @@ fun _ -> binop_expr d <?> "Expression" in
  let expr_list d = comma_separated (d.expr d) <?> "ExpressionList" in
  (* ============================================================
     --------               Declarations                 --------
     ============================================================ *)

  (* TODO: simplified
     There are two types of supported var declarations:
     1. var x = expr;
     2. var x type;

     VarDecl := "var" Ident Initializer ";" .
     Initializer := "=" expr | type .
  *)
  let initializer_ d =
    let with_expr = char '=' *> ws *> d.expr d in
    let with_type =
      let* t = typ in
      return (Make t)
    in
    with_expr <|> with_type
  in
  let var_decl d =
    let* name = keyword "var" *> ws1 *> ident_str <* ws in
    let* value = initializer_ d <* ws <* semi in
    return (name, value)
  in
  let func_decl d =
    (let* _ = keyword "func" *> ws1 in
     let* name = ident_str <* ws in
     let* sign = signature <* ws in
     let* body = d.block d in
     return (name, sign, body))
    <?> "FuncDecl"
  in
  let top_level_func_decl d =
    let* fd = func_decl d in
    return (FuncDecl fd)
  in
  let global_var_decl d =
    let* v = var_decl d in
    return (GlobalVarDecl v)
  in
  let top_level_decl d =
    (fix @@ fun _ -> top_level_func_decl d <|> global_var_decl d) <?> "TopLevelDecl"
  in
  (* ============================================================
     --------               Statements                  --------
     ============================================================ *)

  (* Statement :=
          Declaration | SimpleStmt |
          GoStmt | ReturnStmt | BreakStmt | ContinueStmt |
          Block | IfStmt | SwitchStmt | ForStmt .
     SimpleStatement := ExpressionStmt | SendStmt | Assignment *)
  let declaration_stmt d =
    let* v = var_decl d in
    return (VarDecl v)
  in
  let expr_stmt d =
    let* e = d.expr d <* ws <* semi in
    return (ExprStmt e)
  in
  let assign d =
    let* left = d.expr d <* ws <* char '=' <* ws in
    let* right = d.expr d <* ws <* semi in
    return (AssignStmt (left, right))
  in
  let send_stmt d =
    let* chan = ident in
    let* _ = ws <* string "<-" <* ws in
    let* x = d.expr d <* ws <* semi in
    return (SendStmt (chan, x))
  in
  let simple_stmt d = send_stmt d <|> expr_stmt d <|> assign d in
  let go_stmt d =
    (let* expr = keyword "go" *> ws1 *> d.expr d <* ws <* semi in
     return (GoStmt expr))
    <?> "GoStatement"
  in
  let ret_stmt d =
    let ret_void =
      let* _ = keyword "return" *> ws *> semi in
      return (RetStmt None)
    in
    let ret_expr =
      let* expr = keyword "return" *> ws1 *> expr d <* ws <* semi in
      return (RetStmt (Some expr))
    in
    ret_expr <|> ret_void <?> "ReturnStatement"
  in
  let block d = in_braces (many (ws *> d.stmt d <* ws)) in
  let block_stmt d =
    (let* b = block d in
     return (BlockStmt b))
    <?> "BlockStmt"
  in
  (* Parse else-if's *)
  let if_stmt d =
    (let* cond = keyword "if" *> ws1 *> d.expr d <* ws in
     let* then_block = block d <* ws in
     let* next_char = peek_char_fail in
     if next_char = 'e'
     then
       let* else_block = keyword "else" *> ws *> block d in
       return (IfStmt (cond, then_block, else_block))
     else return (IfStmt (cond, then_block, [])))
    <?> "IfStatement"
  in
  let for_stmt d =
    let* _ = keyword "for" *> ws1 in
    let* expr = d.expr d <* ws in
    let* b = block d in
    return (ForStmt (expr, b))
  in
  let stmt d =
    fix
    @@ fun _ ->
    declaration_stmt d
    <|> go_stmt d
    <|> ret_stmt d
    <|> block_stmt d
    <|> if_stmt d
    <|> for_stmt d
    <|> simple_stmt d
    <?> "Statement"
  in
  { expr_list; expr; var_decl; func_decl; top_level_decl; block; stmt }
;;

let expr_list = eds.expr_list eds
let expr = eds.expr eds
let var_decl = eds.var_decl eds
let func_decl = eds.func_decl eds
let top_level_decl = eds.top_level_decl eds
let block = eds.block eds
let stmt = eds.stmt eds

(* ============================================================
   ---------          Parser entrypoint         ---------------
   ============================================================ *)

let source_file : string source_file t =
  let top_level_decl_list =
    fix
    @@ fun top_level_decl_list ->
    let* finished = at_end_of_input in
    if finished
    then return []
    else
      let* d = top_level_decl <* ws in
      let* tail = top_level_decl_list in
      return (d :: tail)
  in
  top_level_decl_list
;;

let parse_file s = parse source_file s

(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -------                          Tests                        -------
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ~~~~~~~~            Primitive parsers               ~~~~~~~~
   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

let%test _ = Ok "  " = parse ws "  "
let%test _ = Ok "  " = parse ws "  "
let%test _ = Result.is_error (parse ws "123")
let%test _ = Ok "x" = parse ident_str "x"
let%test _ = Ok "_x" = parse ident_str "_x"
let%test _ = Ok "__xyz123" = parse ident_str "__xyz123"
let%test _ = Ok "x112___3232" = parse ident_str "x112___3232"
let%test _ = Result.is_error (parse ident_str "1")
let%test _ = Result.is_error (parse ident_str "1123xy")
let%test _ = Result.is_error (parse ident_str "   1x_y")
let%test _ = Result.is_error (parse ident_str "+1xy")
let%test _ = Ok () = parse_string ~consume:Prefix (eat 'a') "a"
let%test _ = Ok () = parse_string ~consume:Prefix (eat 'a') "b"
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ~~~~~~      String content & escape sequences          ~~~~~
   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
let%test _ = Ok "abc" = parse string_body "abc"
let%test _ = Ok "Привет123  606" = parse string_body "Привет123  606"
let%test _ = Ok "newline \n" = parse string_body {|newline \n|}
let%test _ = Ok "slash \\" = parse string_body {|slash \\|}
let%test _ = Result.is_error (parse string_body "\"Hello\nworld\"")
let%test _ = Result.is_error (parse string_body "\"Hello \" world\"")
let%test _ = Result.is_error (parse string_body "Hello \\z world")
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ~~~~~~~~~~     Constnats & Identifiers           ~~~~~~~~~~~
   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
let%test _ = Ok (Int 123) = parse int_const "123"
let%test _ = Result.is_error (parse int_const "abc")
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ~~~~~~~~~~              Types                    ~~~~~~~~~~~
   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
let%test _ = Ok IntTyp = parse typ "int"
let%test _ = Ok (ArrayTyp { el = IntTyp }) = parse typ "[]int"
let%test _ = Ok (ArrayTyp { el = ArrayTyp { el = IntTyp } }) = parse typ "[][]int"
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ~~~~~~~~           Expressions Tests                ~~~~~~~~
   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
let%test _ = Ok (Const (Int 1)) = parse expr "1"
let%test _ = Ok (Const (Str "string")) = parse expr {|"string"|}
(* Parenthesised constants *)
let%test _ = Ok (Const (Int 1)) = parse expr "(1)"
let%test _ = Ok (Const (Str "string")) = parse expr {|("string")|}
(* Idents *)
let%test _ = Ok (Ident "abc") = parse expr "abc"
let%test _ = Ok (Ident "abc") = parse expr "(abc)"
(* Array Literals *)
let%test _ = Ok (ArrLit ({ el = IntTyp }, [])) = parse expr "[]int{}"

let%test _ =
  Ok (ArrLit ({ el = IntTyp }, [ Const (Int 1); Ident "true"; Const (Str "yes") ]))
  = parse expr {|[]int{1,true,"yes"}|}
;;

(* Index Expressions *)
let%test _ = Ok (ArrIndex (Ident "a", Const (Int 0))) = parse expr "a[0]"

let%test _ =
  Ok (ArrIndex (ArrIndex (Ident "a", Const (Int 0)), Const (Int 1)))
  = parse expr "a[0][1]"
;;

let%test _ =
  Ok (ArrIndex (Ident "a", ArrIndex (Ident "b", Const (Int 0)))) = parse expr "a[b[0]]"
;;

(* Calls *)
let%test _ = Ok (Call (Ident "f", [])) = parse expr "f()"
let%test _ = Ok (Call (Ident "f", [ Const (Int 1) ])) = parse expr "f(1)"

let%test _ =
  Ok (Call (Ident "f", [ Const (Int 1); Const (Str "a"); Ident "true" ]))
  = parse expr {|f(1,"a",true)|}
;;

(* Index and calls mixed *)

let%test _ =
  Ok (Call (ArrIndex (Ident "a", Const (Int 0)), [ Ident "b" ])) = parse expr "a[0](b)"
;;

let%test _ =
  Ok
    (Call
       ( ArrIndex (Ident "a", Call (Ident "f", []))
       , [ ArrIndex (ArrIndex (Ident "b", Const (Int 1)), Const (Int 2)) ] ))
  = parse expr "a[f()](b[1][2])"
;;

let%test _ =
  Ok
    (Call
       ( ArrIndex (Ident "a", Call (Ident "f", [ Const (Str "h"); Const (Int 1) ]))
       , [ ArrIndex (ArrIndex (Ident "b", Const (Int 1)), Const (Int 2)) ] ))
  = parse expr {|a[f("h",1)](b[1][2])|}
;;

(* Function literals *)
let%test _ = Ok (FuncLit ({ args = []; ret = Void }, [])) = parse expr "func(){}"
let%test _ = Ok (FuncLit ({ args = []; ret = Void }, [])) = parse expr "func  (  )   \n{}"

let%test _ =
  Ok (FuncLit ({ args = []; ret = One IntTyp }, [])) = parse expr "func() int {}"
;;

let%test _ =
  Ok
    (FuncLit
       ({ args = [ "x", IntTyp; "y", ArrayTyp { el = IntTyp } ]; ret = One IntTyp }, []))
  = parse expr "func(x int, y []int,) int {}"
;;

let%test _ =
  Ok
    (FuncLit
       ( { args = [ "x", ArrayTyp { el = IntTyp } ]; ret = One (ArrayTyp { el = IntTyp }) }
       , [ RetStmt (Some (Ident "x")) ] ))
  = parse expr "func(x []int,) []int { return x; }"
;;

(* Binary operators *)
(* [
     [ (Mul, "*"); (Div, "/"); (Mod, "%") ];
     [ (Add, "+"); (Sub, "-") ];
     [
       (Eq, "=="); (Neq, "!="); (Lt, "<"); (Lte, "<="); (Gt, ">"); (Gte, ">=");
     ];
     [ (And, "&&") ];
     [ (Or, "||") ];
   ] *)

let%test _ = Ok (BinOp (Const (Int 1), Add, Const (Int 2))) = parse expr "1+2"

let%test _ =
  Ok (BinOp (BinOp (Const (Int 1), Add, Const (Int 2)), Add, Const (Int 3)))
  = parse expr "1+2+3"
;;

let%test _ =
  Ok (BinOp (Const (Int 1), Add, BinOp (Const (Int 2), Add, Const (Int 3))))
  = parse expr "1+(2+3)"
;;

let%test _ =
  Ok (BinOp (Const (Int 1), Add, BinOp (Const (Int 2), Mul, Const (Int 3))))
  = parse expr "1+2*3"
;;

let%test _ =
  Ok
    (BinOp
       ( BinOp (BinOp (Ident "x", Mod, Const (Int 2)), Eq, Const (Int 0))
       , Or
       , BinOp (BinOp (Ident "y", Mod, Const (Int 2)), Eq, Const (Int 0)) ))
  = parse expr "x % 2 == 0 || y % 2 == 0"
;;

let%test _ =
  Ok (BinOp (Ident "a", Eq, BinOp (Ident "b", And, Ident "c")))
  = parse expr "a == (b && c)"
;;

(* Unary operators *)

let%test _ = Ok (UnOp (Minus, Ident "a")) = parse expr "-a"
let%test _ = Ok (UnOp (Not, Ident "b")) = parse expr "!b"
let%test _ = Ok (BinOp (UnOp (Minus, Ident "a"), Mul, Ident "b")) = parse expr "-a*b"

let%test _ =
  Ok (BinOp (UnOp (Minus, Ident "a"), Mul, UnOp (Not, Ident "b"))) = parse expr "-a*!b"
;;

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ~~~~~~~~           Declarations Tests               ~~~~~~~~
   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
let%test _ = Ok ("x", Make IntTyp) = parse var_decl "var x int;"
let%test _ = Ok ("y", Const (Int 1)) = parse var_decl "var y=1;"
let%test _ = Ok ("y", Ident "x") = parse var_decl "var y=x;"
let%test _ = Ok ("f", { args = []; ret = Void }, []) = parse func_decl "func f() {}"

let%test _ =
  Ok ("f", { args = []; ret = One IntTyp }, []) = parse func_decl "func f() int {}"
;;

let%test _ =
  Ok ("f", { args = [ "x", IntTyp ]; ret = One IntTyp }, [])
  = parse func_decl "func f(x int) int {}"
;;

let%test _ =
  Ok ("f", { args = [ "x", IntTyp; "y", IntTyp ]; ret = One IntTyp }, [])
  = parse func_decl "func f(x int,y int) int {}"
;;

let%test _ =
  Ok ("f", { args = [ "x", IntTyp; "y", IntTyp ]; ret = Void }, [])
  = parse func_decl "func f(x int,y int)  {}"
;;

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ~~~~~~~~             Statement Tests                ~~~~~~~~
   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* var declarations *)
let%test _ = Ok (VarDecl ("x", Const (Int 1))) = parse stmt "var x = 1;"

let%test _ =
  Ok (VarDecl ("abc", ArrLit ({ el = IntTyp }, [ Const (Int 2) ])))
  = parse stmt "var abc = []int{2};"
;;

let%test _ =
  Ok (VarDecl ("abc", ArrLit ({ el = IntTyp }, [ Const (Int 2) ])))
  = parse stmt "var abc = [] int   {2};"
;;

(* expression statements *)
let%test _ = Ok (ExprStmt (Const (Int 1))) = parse stmt "1;"
let%test _ = Ok (ExprStmt (Ident "foo")) = parse stmt "foo;"

let%test _ =
  Ok (ExprStmt (Call (Ident "f", [ ArrLit ({ el = IntTyp }, [ Const (Str "123") ]) ])))
  = parse stmt {|f([]int{"123"});|}
;;

(* assignments *)
let%test _ = Ok (AssignStmt (Ident "a", Call (Ident "ff", []))) = parse stmt "a = ff();"
(* channel send *)
let%test _ = Ok (SendStmt (Ident "c", Ident "a")) = parse stmt "c <- a;"
(* statements that start with a keyword *)
let%test _ = Ok (GoStmt (Call (Ident "mygoroutine", []))) = parse stmt "go mygoroutine();"
(* block *)
let%test _ = Ok (BlockStmt []) = parse stmt "{}"

let%test _ =
  Ok (BlockStmt [ ExprStmt (Ident "a"); ExprStmt (Ident "b") ]) = parse stmt "{a;\nb;}"
;;

let%test _ =
  Ok (BlockStmt [ ExprStmt (Ident "a"); ExprStmt (Ident "b") ]) = parse stmt "{a;    b;}"
;;

let%test _ =
  Ok
    (BlockStmt
       [ ExprStmt (Ident "a")
       ; ExprStmt (Const (Int 1))
       ; ExprStmt (Call (Call (Ident "foo", [ Ident "bar" ]), [ Ident "zar" ]))
       ])
  = parse stmt "{a;    1; foo(bar)(zar);}"
;;

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ~~~~~~~~               Insane Tests                 ~~~~~~~~
   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

let%test _ =
  Ok
    ( "f"
    , { args = [ "xx", IntTyp; "yy", IntTyp ]; ret = One IntTyp }
    , [ VarDecl ("a", Const (Int 1))
      ; ExprStmt (Call (Ident "print", [ Ident "a"; Const (Int 2) ]))
      ; RetStmt (Some (Ident "a"))
      ] )
  = parse
      func_decl
      {|func f(xx int, yy int) int { 
        var a = 1;
        print(a, 2);
        return a;
      }|}
;;

let%test _ =
  Ok
    ( "f"
    , { args = [ "xx", IntTyp; "yy", IntTyp ]; ret = One IntTyp }
    , [ VarDecl ("a", Const (Int 1))
      ; ExprStmt (Call (Ident "print", [ Ident "a"; Const (Int 2) ]))
      ; BlockStmt
          [ VarDecl ("b", Const (Int 10))
          ; VarDecl ("c", Make StrTyp)
          ; ExprStmt (Call (Ident "f", [ Ident "b" ]))
          ]
      ; RetStmt (Some (Ident "a"))
      ] )
  = parse
      func_decl
      {|func f(xx int, yy int) int { 
            var a = 1;
            print(a, 2);
            { var b = 10; var c string; f(b); }
            return a;
          }|}
;;
