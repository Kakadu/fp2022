(** Copyright 2021-2022, Kakadu, EmirVildanov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Parser module *)

open Angstrom
open Ast
open Typing

type error = [ `ParsingError of string ]

let pp_error ppf = function
  | `ParsingError s -> Format.fprintf ppf "%s" s
;;

module Is = struct
  let is_whitespace = function
    | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
    | _ -> false
  ;;

  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false
  ;;

  let is_alpha = function
    | 'a' .. 'z' -> true
    | _ -> false
  ;;

  let is_capital = function
    | 'A' .. 'Z' -> true
    | _ -> false
  ;;
end

let parens_covered_p inner_p = char '(' *> inner_p <* char ')'
let whitespace_p = take_while Is.is_whitespace
let mand_whitespace_p = take_while1 Is.is_whitespace

(* List of keywords that variable can't be named with *)
let keywords_list =
  [ "if"
  ; "then"
  ; "else"
  ; "let"
  ; "rec"
  ; "true"
  ; "false"
  ; "match"
  ; "with"
  ; "in"
  ; "fun"
  ; "type"
  ; "int"
  ; "string"
  ; "bool"
  ]
;;

(* Dispatch for parsing expresstions *)
type dispatch =
  { tuple_p : dispatch -> expr Angstrom.t
  ; cons_p : dispatch -> expr Angstrom.t
  ; binop_p : dispatch -> expr Angstrom.t
  ; unary_p : dispatch -> expr Angstrom.t
  ; list_p : dispatch -> expr Angstrom.t
  ; app_p : dispatch -> expr Angstrom.t
  ; fun_p : dispatch -> expr Angstrom.t
  ; if_then_else_p : dispatch -> expr Angstrom.t
  ; match_p : dispatch -> expr Angstrom.t
  ; let_in_p : dispatch -> expr Angstrom.t
  ; let_p : dispatch -> expr Angstrom.t
  ; adt_p : dispatch -> expr Angstrom.t
  ; expr_p : dispatch -> expr Angstrom.t
  }

(* Dispatch for parsing tupes *)
type dispatch_t =
  { base_t_p : Typing.t Angstrom.t
  ; custom_t_p : dispatch_t -> Typing.t Angstrom.t
  ; tuple_t_p : dispatch_t -> Typing.t Angstrom.t
  ; arrow_t_p : dispatch_t -> Typing.t Angstrom.t
  ; t_p : dispatch_t -> Typing.t Angstrom.t
  }

(* Helper for variable name parser *)
let id_p =
  take_while1 Is.is_alpha
  >>= fun value ->
  if List.mem value keywords_list then fail "The id is keyword" else return value
;;

(* ADT name (Capital name) parser *)
let adt_name_p =
  peek_char_fail
  >>= fun char ->
  match Is.is_capital char with
  | true ->
    advance 1
    >>= fun () ->
    take_while Is.is_alpha >>| fun after_capital -> String.make 1 char ^ after_capital
  | false -> fail "Capital letter expected"
;;

(* Type parsers. They are going first, because Let expression parser
   will need to use them *)
(* Parser of Base type *)
let base_t_p =
  fix (fun p ->
    let number_p = string "int" *> return number_t in
    let bool_p = string "bool" *> return bool_t in
    let string_p = string "string" *> return string_t in
    let unit_p = string "()" *> return unit_t in
    choice [ parens_covered_p p; number_p; bool_p; string_p; unit_p ])
;;

(* Parser of Tuple type *)
let tuple_t_p td =
  fix (fun fixed ->
    whitespace_p
    *> (parens_covered_p fixed
       <|> (sep_by1
              (whitespace_p *> string "*" <* whitespace_p)
              (choice
                 [ parens_covered_p (td.arrow_t_p td)
                 ; parens_covered_p fixed
                 ; td.custom_t_p td
                 ; base_t_p
                 ])
           >>= fun typ_list ->
           whitespace_p *> option "" (string "list")
           >>= function
           | "" ->
             if Base.List.length typ_list > 1
             then return (tuple_t typ_list)
             else fail "Fail during tuple type parsing!"
           | _ -> fail "Fail during tuple type parsing!")))
;;

(* Parser of Arrow type *)
let arrow_t_p td =
  fix (fun fixed ->
    whitespace_p
    *> (parens_covered_p fixed
       <|> lift2
             (fun left right -> ArrowT (left, right))
             (choice
                [ parens_covered_p fixed; td.custom_t_p td; td.tuple_t_p td; base_t_p ])
             (whitespace_p *> string "->" *> whitespace_p *> td.t_p td)))
;;

(* Parser of Custom (like `color` or `weight`) type including
   builin types like `option` and `list` *)
let custom_t_p td =
  fix (fun fixed ->
    whitespace_p
    *> (parens_covered_p fixed
       <|> lift2
             (fun t name -> NamedT (name, Some t))
             (choice
                [ parens_covered_p (td.arrow_t_p td)
                ; parens_covered_p (td.tuple_t_p td)
                ; base_t_p
                ]
             <* whitespace_p)
             id_p
       <|> ((fun name -> NamedT (name, None)) <$> id_p)))
;;

(* Parser of general type *)
let t_p td =
  fix (fun fixed ->
    whitespace_p
    *> (choice [ td.arrow_t_p td; td.custom_t_p td; td.tuple_t_p td; base_t_p ]
       <|> parens_covered_p fixed))
;;

(* Right side of ADT type declaration *)
let adt_decalaration_right_part_p td =
  let stick_p = whitespace_p *> string "|" <* mand_whitespace_p in
  let choice_t_p =
    choice [ td.arrow_t_p td; td.custom_t_p td; td.tuple_t_p td; base_t_p ]
  in
  let variant =
    (fun name t -> name, t)
    <$> adt_name_p
    <*> option
          None
          ((fun t -> Some t)
          <$> mand_whitespace_p *> string "of" *> mand_whitespace_p *> choice_t_p)
  in
  (fun variant -> AdtT variant) <$> option "" stick_p *> sep_by stick_p variant
;;

(* Default type dispatch *)
let default_d_t = { base_t_p; tuple_t_p; arrow_t_p; custom_t_p; t_p }

(* General type declatation using `type` keyword *)
let type_declaration_p =
  (string "type" <* mand_whitespace_p)
  *> lift2
       (fun name t -> Type (name, t))
       id_p
       (string "=" *> (t_p default_d_t <|> adt_decalaration_right_part_p default_d_t))
;;

(* Expression parsers *)
(* Parser of Var expression *)
let var_p = (fun x -> Var x) <$> id_p

(* Parser of Constant expression *)
let const_p =
  let number_p =
    take_while1 Is.is_digit >>= fun number -> return (Ast.Int (int_of_string number))
  in
  let bool_p =
    (fun _ -> Ast.Bool true)
    <$> string "true"
    <|> ((fun _ -> Ast.Bool false) <$> string "false")
  in
  let string_p = (fun s -> Ast.Str s) <$> (char '"' *> id_p <* char '"') in
  let unit_p = (fun _ -> Ast.Unit) <$> string "()" in
  let nil_p = (fun _ -> Ast.Nil) <$> string "[]" in
  (fun x -> Constant x) <$> (number_p <|> bool_p <|> nil_p <|> unit_p <|> string_p)
;;

(* Parser of ADT expression *)
let adt_p d =
  let chain_p fixed =
    choice
      [ fixed
      ; d.list_p d
      ; d.binop_p d
      ; d.unary_p d
      ; d.list_p d
      ; d.app_p d
      ; d.fun_p d
      ; d.if_then_else_p d
      ; d.match_p d
      ; d.let_in_p d
      ; d.adt_p d
      ; const_p
      ; var_p
      ]
  in
  fix (fun p ->
    let ps = chain_p p in
    (fun name args -> ADT (name, args))
    <$> adt_name_p
    <*> option [] (mand_whitespace_p *> parens_covered_p (sep_by1 (char ',') ps)))
;;

(* Parser of Tuple expression *)
let tuple_p d =
  let choice_p fixed =
    choice
      [ parens_covered_p fixed
      ; d.list_p d
      ; d.binop_p d
      ; d.unary_p d
      ; d.list_p d
      ; d.app_p d
      ; d.fun_p d
      ; d.if_then_else_p d
      ; d.match_p d
      ; d.let_in_p d
      ; d.adt_p d
      ; const_p
      ; var_p
      ]
  in
  parens_covered_p
    (fix (fun p ->
       let ps = choice_p p in
       (fun first other -> Ast.Tuple (first :: other))
       <$> (ps <* char ',')
       <*> sep_by1 (char ',') ps))
;;

(* Parser of List expression *)
let list_p d =
  let brackets p = char '[' *> p <* char ']' in
  let choice_p fixed =
    choice
      [ d.tuple_p d
      ; d.cons_p d
      ; d.binop_p d
      ; d.unary_p d
      ; fixed
      ; d.app_p d
      ; d.fun_p d
      ; d.if_then_else_p d
      ; d.match_p d
      ; d.let_in_p d
      ; d.adt_p d
      ; const_p
      ; var_p
      ]
  in
  fix (fun p ->
    let ps = choice_p p in
    let rec unfold_expr_list = function
      | [] -> ADT (nil_adt_name, [])
      | [ x ] -> ADT (cons_adt_name, [ x; unfold_expr_list [] ])
      | h :: t -> ADT (cons_adt_name, [ h; unfold_expr_list t ])
    in
    (fun exprs -> unfold_expr_list exprs) <$> brackets (sep_by1 (char ';') ps))
;;

(* Parser of UnaryOp expression *)
let unary_p d =
  let negation = char '-' in
  let choice_p fixed =
    choice
      [ parens_covered_p fixed
      ; parens_covered_p (d.app_p d)
      ; parens_covered_p (d.if_then_else_p d)
      ; parens_covered_p (d.match_p d)
      ; parens_covered_p (d.let_in_p d)
      ; const_p
      ; var_p
      ; parens_covered_p (d.binop_p d)
      ]
  in
  fix (fun p ->
    let ps = choice_p p in
    (fun expr -> UnaryOp (UnaryMinus, expr)) <$> negation *> ps)
;;

(* Parser of BinaryOp expression.
   For understanding of `chainl1`, `factor` and `term` see Angstrom GitHub repo example:
   https://github.com/inhabitedtype/angstrom#usage *)
let binop_p d =
  let mul_p = (fun _ -> Mult) <$> char '*' in
  let add_p = (fun _ -> Plus) <$> char '+' in
  let sub_p = (fun _ -> Minus) <$> char '-' in
  let div_p = (fun _ -> Divide) <$> char '/' in
  let eq_p = (fun _ -> Eq) <$> char '=' in
  let chainl1 e op =
    let rec go acc = lift2 (fun f x -> BinaryOp (f, acc, x)) op e >>= go <|> return acc in
    e >>= fun init -> go init
  in
  let choice_p fixed =
    choice
      [ parens_covered_p fixed
      ; d.cons_p d
      ; d.unary_p d
      ; d.list_p d
      ; d.app_p d
      ; d.if_then_else_p d
      ; d.match_p d
      ; d.let_in_p d
      ; d.adt_p d
      ; const_p
      ; var_p
      ]
  in
  fix (fun p ->
    let ps = choice_p p in
    let factor = ps in
    let priority1 = mul_p <|> div_p in
    let priority2 = add_p <|> sub_p in
    let priority3 = eq_p in
    let term1 = chainl1 factor priority1 in
    let term2 = chainl1 term1 priority2 in
    chainl1 term2 priority3)
;;

(* Parser of IfThenElse expression *)
let if_then_else_p d =
  let if_then_else_p = string "if" <* mand_whitespace_p in
  let then_p = mand_whitespace_p *> string "then" <* mand_whitespace_p in
  let else_p = mand_whitespace_p *> string "else" <* mand_whitespace_p in
  let choice_p fixed =
    choice
      [ d.tuple_p d
      ; d.cons_p d
      ; d.binop_p d
      ; d.unary_p d
      ; d.list_p d
      ; d.app_p d
      ; d.fun_p d
      ; fixed
      ; d.match_p d
      ; d.let_in_p d
      ; d.adt_p d
      ; const_p
      ; var_p
      ]
  in
  fix (fun p ->
    let ps = choice_p p in
    (fun _ if_expr _ then_expr else_expr -> IfThenElse (if_expr, then_expr, else_expr))
    <$> if_then_else_p
    <*> ps
    <*> then_p
    <*> ps
    <*> option (Constant Unit) (lift2 (fun _ expr -> expr) else_p ps))
;;

(* Parser of Fun expression *)
let fun_p d =
  let fun_p = string "fun" <* mand_whitespace_p in
  let arrow_p = mand_whitespace_p *> string "->" <* mand_whitespace_p in
  let choice_p fixed =
    choice
      [ parens_covered_p fixed
      ; d.tuple_p d
      ; d.cons_p d
      ; d.binop_p d
      ; d.unary_p d
      ; d.list_p d
      ; d.app_p d
      ; fixed
      ; d.if_then_else_p d
      ; d.match_p d
      ; d.let_in_p d
      ; d.adt_p d
      ; const_p
      ; var_p
      ]
  in
  fix (fun p ->
    let ps = choice_p p in
    let rec desugar_fun fn xs arr e =
      match xs with
      | [] -> Constant Unit
      | [ x ] -> Fun (x, e)
      | x :: t -> Fun (x, desugar_fun fn t arr e)
    in
    desugar_fun
    <$> fun_p
    <*> sep_by whitespace_p id_p
    <*> arrow_p
    <*> ps
    >>= fun expr ->
    match expr with
    | Fun (_, _) -> return expr
    | _ -> fail "Error during fun parsing.")
;;

(* Parser of Let expression *)
let let_p d =
  let let_p = string "let" <* mand_whitespace_p in
  let rec_p = string "rec" <* mand_whitespace_p >>= fun _ -> return true in
  let assignment_p = mand_whitespace_p *> string "=" <* mand_whitespace_p in
  let choice_p _ =
    choice
      [ d.tuple_p d
      ; d.cons_p d
      ; d.binop_p d
      ; d.unary_p d
      ; d.list_p d
      ; d.app_p d
      ; d.fun_p d
      ; d.if_then_else_p d
      ; d.match_p d
      ; d.let_in_p d
      ; d.adt_p d
      ; const_p
      ; var_p
      ]
  in
  let rec handle_function : id list -> expr -> expr =
   fun xs expr ->
    match xs with
    | [] -> Constant (Str "Empty Fun arguments list during Let parsing!")
    | [ x ] -> Fun (x, expr)
    | x :: t -> Fun (x, handle_function t expr)
  in
  let handle_let flag xs expr =
    match xs with
    | [] -> Constant (Str "Empty Let argumnets list!")
    | [ x ] -> Let (flag, x, None, expr)
    | x :: t ->
      let function_handling_result = handle_function t expr in
      (match function_handling_result with
       | Constant (Str _) -> function_handling_result
       | _ -> Let (flag, x, None, function_handling_result))
  in
  let match_id_list : id -> bool -> id list -> id -> expr -> expr =
   fun _ flag xs _ expr1 -> handle_let flag xs expr1
  in
  let match_typed_list : bool -> id -> Typing.t -> expr -> expr =
   fun flag name t expr -> Let (flag, name, Some t, expr)
  in
  fix (fun p ->
    let ps = choice_p p in
    match_id_list
    <$> let_p
    <*> option false rec_p
    <*> sep_by1 whitespace_p id_p
    <*> assignment_p
    <*> ps
    <|> (match_typed_list
        <$> let_p *> option false rec_p
        <*> id_p
        <*> string ":" *> t_p default_d_t
        <*> assignment_p *> ps)
    >>= fun expr ->
    match expr with
    | Constant (Str message) -> fail message
    | _ -> return expr)
;;

(* Parser of LetIn expression *)
let let_in_p d =
  let in_p = mand_whitespace_p *> string "in" <* mand_whitespace_p in
  let choice_p _ =
    choice
      [ d.tuple_p d
      ; d.cons_p d
      ; d.binop_p d
      ; d.unary_p d
      ; d.list_p d
      ; d.app_p d
      ; d.fun_p d
      ; d.if_then_else_p d
      ; d.match_p d
      ; d.let_in_p d
      ; d.adt_p d
      ; const_p
      ; var_p
      ]
  in
  fix (fun p ->
    let ps = choice_p p in
    let after_in_part = (fun _ exp -> exp) <$> in_p <*> ps in
    (fun let_expr after_in_expr -> LetIn (let_expr, after_in_expr))
    <$> let_p d
    <*> after_in_part)
;;

(* Parser of Cons expression *)
let cons_p d =
  let dots = string "::" in
  let choice_p fixed =
    choice
      [ parens_covered_p (d.tuple_p d)
      ; parens_covered_p fixed
      ; parens_covered_p (d.binop_p d)
      ; d.unary_p d
      ; d.list_p d
      ; d.app_p d
      ; parens_covered_p (d.fun_p d)
      ; parens_covered_p (d.if_then_else_p d)
      ; parens_covered_p (d.match_p d)
      ; d.let_in_p d
      ; d.adt_p d
      ; const_p
      ; var_p
      ]
  in
  fix (fun p ->
    let ps = choice_p p in
    (fun first second -> Cons (first, second)) <$> ps <* dots <*> ps)
;;

(* Parser of Match expression *)
let match_p d =
  let match_p = string "match" <* mand_whitespace_p in
  let with_p = mand_whitespace_p *> string "with" <* mand_whitespace_p in
  let stick_p = whitespace_p *> string "|" <* mand_whitespace_p in
  let arrow_p = mand_whitespace_p *> string "->" <* mand_whitespace_p in
  let matching_expr_choice_p fixed =
    choice
      [ d.tuple_p d
      ; d.cons_p d
      ; d.binop_p d
      ; d.unary_p d
      ; d.list_p d
      ; d.app_p d
      ; d.fun_p d
      ; d.if_then_else_p d
      ; fixed
      ; d.let_in_p d
      ; d.adt_p d
      ; const_p
      ; var_p
      ]
  in
  let pattern_expr_choice_p _ =
    choice [ d.tuple_p d; d.cons_p d; d.unary_p d; d.list_p d; d.adt_p d; const_p; var_p ]
  in
  let match_branch_p match_ps =
    fix (fun p ->
      let pattern_ps = pattern_expr_choice_p p in
      (fun _ pt _ expr -> pt, expr) <$> stick_p <*> pattern_ps <*> arrow_p <*> match_ps)
  in
  let match_branch_with_or_withoutout_stick_p match_ps =
    fix (fun p ->
      let pattern_ps = pattern_expr_choice_p p in
      (fun _ pt _ expr -> pt, expr)
      <$> option "" stick_p
      <*> pattern_ps
      <*> arrow_p
      <*> match_ps)
  in
  fix (fun p ->
    let match_ps = matching_expr_choice_p p in
    (fun _ x _ first ps -> Match (x, [ first ] @ ps))
    <$> match_p
    <*> match_ps
    <*> with_p
    <*> match_branch_with_or_withoutout_stick_p match_ps
    <*> sep_by whitespace_p (match_branch_p match_ps))
;;

(* Parser of App expression *)
let app_p d =
  let func_choice_p _ =
    choice
      [ parens_covered_p (d.fun_p d)
      ; parens_covered_p (d.if_then_else_p d)
      ; parens_covered_p (d.match_p d)
      ; parens_covered_p (d.let_in_p d)
      ; var_p
      ]
  in
  let args_choice_p fixed =
    choice
      [ tuple_p d
      ; parens_covered_p (d.tuple_p d)
      ; parens_covered_p (d.cons_p d)
      ; (* If we leave binop with no braces case, it will
           somewhy identify arguments as inner application *)
        parens_covered_p (d.binop_p d)
      ; d.unary_p d
      ; parens_covered_p (d.unary_p d)
      ; d.list_p d
      ; parens_covered_p fixed
      ; parens_covered_p (d.fun_p d)
      ; parens_covered_p (d.if_then_else_p d)
      ; parens_covered_p (d.match_p d)
      ; parens_covered_p (d.let_in_p d)
      ; parens_covered_p (d.adt_p d)
      ; const_p
      ; var_p
      ]
  in
  let rec parse_exprs = function
    | [] -> Constant (Str "No arguments for App!")
    | [ _ ] -> Constant (Str "Only one argument for App!")
    | [ x; y ] -> App (y, x)
    | h :: t -> App (parse_exprs t, h)
  in
  fix (fun p ->
    let func_ps = func_choice_p p in
    let args_ps = args_choice_p p in
    (fun func exprs -> parse_exprs (List.rev (func :: exprs)))
    <$> func_ps
    <* mand_whitespace_p
    <*> sep_by1 whitespace_p args_ps
    >>= fun expr ->
    match expr with
    | Constant (Str message) -> fail message
    | _ -> return expr)
;;

(* Parser of general expression *)
let expr_p d =
  choice
    [ d.tuple_p d
    ; d.cons_p d
    ; d.binop_p d
    ; d.unary_p d
    ; d.list_p d
    ; d.app_p d
    ; d.fun_p d
    ; d.if_then_else_p d
    ; d.match_p d
    ; d.let_in_p d
    ; d.let_p d
    ; d.adt_p d
    ; const_p
    ; var_p
    ]
;;

(* Default expresstion dispatch *)
let default_d_p =
  { tuple_p
  ; cons_p
  ; binop_p
  ; unary_p
  ; list_p
  ; app_p
  ; fun_p
  ; if_then_else_p
  ; match_p
  ; let_p
  ; let_in_p
  ; adt_p
  ; expr_p
  }
;;

let parse str =
  match parse_string ~consume:All (expr_p default_d_p <|> type_declaration_p) str with
  | Result.Ok v -> Result.Ok v
  | Error msg -> Result.Error (`ParsingError msg)
;;

let parse_optimistically str = Result.get_ok (parse str)
let pp = Ast.pp_expr

(* Constant *)
let%expect_test "Integer" =
  Format.printf "%a" pp (parse_optimistically "2");
  [%expect {| (Constant (Int 2)) |}]
;;

let%expect_test "Negative integer" =
  Format.printf "%a" pp (parse_optimistically "-2");
  [%expect {| (UnaryOp (UnaryMinus, (Constant (Int 2)))) |}]
;;

let%expect_test "Negative integer in brackets" =
  Format.printf "%a" pp (parse_optimistically "(-(2))");
  [%expect {| (UnaryOp (UnaryMinus, (Constant (Int 2)))) |}]
;;

let%expect_test "Variable" =
  Format.printf "%a" pp (parse_optimistically "x");
  [%expect {| (Var "x") |}]
;;

let%expect_test "Boolean" =
  Format.printf "%a" pp (parse_optimistically "true");
  [%expect {| (Constant (Bool true)) |}]
;;

(* BinaryOp *)
let%expect_test "Simple sum" =
  Format.printf "%a" pp (parse_optimistically "3+4");
  [%expect {| (BinaryOp (Plus, (Constant (Int 3)), (Constant (Int 4)))) |}]
;;

let%expect_test "Simple sum with unary negation" =
  Format.printf "%a" pp (parse_optimistically "-3+4");
  [%expect
    {|
      (BinaryOp (Plus, (UnaryOp (UnaryMinus, (Constant (Int 3)))),
         (Constant (Int 4)))) |}]
;;

let%expect_test "Equality" =
  Format.printf "%a" pp (parse_optimistically "x=3");
  [%expect {| (BinaryOp (Eq, (Var "x"), (Constant (Int 3)))) |}]
;;

let%expect_test "Sum of mult and var" =
  Format.printf "%a" pp (parse_optimistically "3*x+y");
  [%expect
    {|
    (BinaryOp (Plus, (BinaryOp (Mult, (Constant (Int 3)), (Var "x"))), (Var "y")
       )) |}]
;;

let%expect_test "Sum of mult and int" =
  Format.printf "%a" pp (parse_optimistically "3*2+1");
  [%expect
    {|
    (BinaryOp (Plus, (BinaryOp (Mult, (Constant (Int 3)), (Constant (Int 2)))),
       (Constant (Int 1)))) |}]
;;

let%expect_test "Equality of var and div" =
  Format.printf "%a" pp (parse_optimistically "x=y/3");
  [%expect
    {|
    (BinaryOp (Eq, (Var "x"), (BinaryOp (Divide, (Var "y"), (Constant (Int 3))))
       )) |}]
;;

(* ADT *)
let%expect_test "None" =
  Format.printf "%a" pp (parse_optimistically "None");
  [%expect {| (ADT ("None", [])) |}]
;;

let%expect_test "Result list" =
  Format.printf "%a" pp (parse_optimistically "Result (1)");
  [%expect {| (ADT ("Result", [(Constant (Int 1))])) |}]
;;

let%expect_test "Custom variant with several arguments" =
  Format.printf "%a" pp (parse_optimistically "Smth (1,[1;2;3],\"aba\")");
  [%expect
    {|
      (ADT ("Smth",
         [(Constant (Int 1));
           (ADT ("Cons",
              [(Constant (Int 1));
                (ADT ("Cons",
                   [(Constant (Int 2));
                     (ADT ("Cons", [(Constant (Int 3)); (ADT ("Nil", []))]))]
                   ))
                ]
              ));
           (Constant (Str "aba"))]
         )) |}]
;;

(* App *)
let%expect_test "Function application with one argument" =
  Format.printf "%a" pp (parse_optimistically "f x");
  [%expect {| (App ((Var "f"), (Var "x"))) |}]
;;

let%expect_test "Function application with three argument" =
  Format.printf "%a" pp (parse_optimistically "f x y z");
  [%expect {| (App ((App ((App ((Var "f"), (Var "x"))), (Var "y"))), (Var "z"))) |}]
;;

let%expect_test "Function application with two nested expressions" =
  Format.printf "%a" pp (parse_optimistically "f (x+3) (y 1 2 3)");
  [%expect
    {|
      (App ((App ((Var "f"), (BinaryOp (Plus, (Var "x"), (Constant (Int 3)))))),
         (App ((App ((App ((Var "y"), (Constant (Int 1)))), (Constant (Int 2)))),
            (Constant (Int 3))))
         )) |}]
;;

(* Tuple *)
let%expect_test "Simple tuple" =
  Format.printf "%a" pp (parse_optimistically "(1,2)");
  [%expect {| (Tuple [(Constant (Int 1)); (Constant (Int 2))]) |}]
;;

let%expect_test "Tuple with nested lis" =
  Format.printf "%a" pp (parse_optimistically "(1,[1;2])");
  [%expect
    {|
        (Tuple
           [(Constant (Int 1));
             (ADT ("Cons",
                [(Constant (Int 1));
                  (ADT ("Cons", [(Constant (Int 2)); (ADT ("Nil", []))]))]
                ))
             ]) |}]
;;

(* List *)
let%expect_test "Simple list" =
  Format.printf "%a" pp (parse_optimistically "[1;2]");
  [%expect
    {|
    (ADT ("Cons",
       [(Constant (Int 1));
         (ADT ("Cons", [(Constant (Int 2)); (ADT ("Nil", []))]))]
       )) |}]
;;

let%expect_test "List of if_then" =
  Format.printf "%a" pp (parse_optimistically "[if 1 then 1;if 2 then 2]");
  [%expect
    {|
      (ADT ("Cons",
         [(IfThenElse ((Constant (Int 1)), (Constant (Int 1)), (Constant Unit)));
           (ADT ("Cons",
              [(IfThenElse ((Constant (Int 2)), (Constant (Int 2)), (Constant Unit)
                  ));
                (ADT ("Nil", []))]
              ))
           ]
         )) |}]
;;

(* Cons *)
let%expect_test "Simple cons" =
  Format.printf "%a" pp (parse_optimistically "1::[1]");
  [%expect
    {|
  (Cons ((Constant (Int 1)),
     (ADT ("Cons", [(Constant (Int 1)); (ADT ("Nil", []))])))) |}]
;;

let%expect_test "Cons of two lists" =
  Format.printf "%a" pp (parse_optimistically "[2;3]::[1;4]");
  [%expect
    {|
          (Cons (
             (ADT ("Cons",
                [(Constant (Int 2));
                  (ADT ("Cons", [(Constant (Int 3)); (ADT ("Nil", []))]))]
                )),
             (ADT ("Cons",
                [(Constant (Int 1));
                  (ADT ("Cons", [(Constant (Int 4)); (ADT ("Nil", []))]))]
                ))
             )) |}]
;;

(* IfThenElse *)
let%expect_test "IfThen" =
  Format.printf "%a" pp (parse_optimistically "if true then x");
  [%expect {| (IfThenElse ((Constant (Bool true)), (Var "x"), (Constant Unit))) |}]
;;

let%expect_test "IfThenElse" =
  Format.printf "%a" pp (parse_optimistically "if x then y else z");
  [%expect {| (IfThenElse ((Var "x"), (Var "y"), (Var "z"))) |}]
;;

(* Fun *)
let%expect_test "Single Fun expression" =
  Format.printf "%a" pp (parse_optimistically "fun x -> x+1");
  [%expect {| (Fun ("x", (BinaryOp (Plus, (Var "x"), (Constant (Int 1)))))) |}]
;;

let%expect_test "Sugar Fun expression with 2 arguments" =
  Format.printf "%a" pp (parse_optimistically "fun x y -> x+y");
  [%expect {| (Fun ("x", (Fun ("y", (BinaryOp (Plus, (Var "x"), (Var "y"))))))) |}]
;;

let%expect_test "Sugar Fun expression with 3 arguments" =
  Format.printf "%a" pp (parse_optimistically "fun x y z -> x");
  [%expect {| (Fun ("x", (Fun ("y", (Fun ("z", (Var "x"))))))) |}]
;;

(* Let + LetIn *)
let%expect_test "Let expression" =
  Format.printf "%a" pp (parse_optimistically "let x = y");
  [%expect {| (Let (false, "x", None, (Var "y"))) |}]
;;

let%expect_test "Let in expression" =
  Format.printf "%a" pp (parse_optimistically "let x = y in z");
  [%expect {| (LetIn ((Let (false, "x", None, (Var "y"))), (Var "z"))) |}]
;;

let%expect_test "Let expression with function" =
  Format.printf "%a" pp (parse_optimistically "let f x = x");
  [%expect {| (Let (false, "f", None, (Fun ("x", (Var "x"))))) |}]
;;

let%expect_test "Let in expression with function" =
  Format.printf "%a" pp (parse_optimistically "let f x = x in z");
  [%expect {| (LetIn ((Let (false, "f", None, (Fun ("x", (Var "x"))))), (Var "z"))) |}]
;;

let%expect_test "Let in expression with function with two args" =
  Format.printf "%a" pp (parse_optimistically "let f x y = x in z");
  [%expect
    {|
        (LetIn ((Let (false, "f", None, (Fun ("x", (Fun ("y", (Var "x"))))))),
           (Var "z"))) |}]
;;

let%expect_test "LetRec expression" =
  Format.printf "%a" pp (parse_optimistically "let rec f x = y");
  [%expect {| (Let (true, "f", None, (Fun ("x", (Var "y"))))) |}]
;;

let%expect_test "Let expression with typying of int" =
  Format.printf "%a" pp (parse_optimistically "let x: int = 1");
  [%expect {| (Let (false, "x", (Some (BaseT Int)), (Constant (Int 1)))) |}]
;;

let%expect_test "Let expression with typying of fun" =
  Format.printf "%a" pp (parse_optimistically "let f: int -> int = fun x -> x+1");
  [%expect
    {|
    (Let (false, "f", (Some (ArrowT ((BaseT Int), (BaseT Int)))),
       (Fun ("x", (BinaryOp (Plus, (Var "x"), (Constant (Int 1)))))))) |}]
;;

let%expect_test "LetIn expression with typying of fun" =
  Format.printf
    "%a"
    pp
    (parse_optimistically "let f: int -> int = fun x -> x+1 in (f x)*3");
  [%expect
    {|
    (LetIn (
       (Let (false, "f", (Some (ArrowT ((BaseT Int), (BaseT Int)))),
          (Fun ("x", (BinaryOp (Plus, (Var "x"), (Constant (Int 1)))))))),
       (BinaryOp (Mult, (App ((Var "f"), (Var "x"))), (Constant (Int 3)))))) |}]
;;

(* Match *)
let%expect_test "Match with single branch and without stick" =
  Format.printf "%a" pp (parse_optimistically "match x with 1 -> 2");
  [%expect {| (Match ((Var "x"), [((Constant (Int 1)), (Constant (Int 2)))])) |}]
;;

let%expect_test "Match with single branch and stick" =
  Format.printf "%a" pp (parse_optimistically "match x with | 1 -> 2");
  [%expect {| (Match ((Var "x"), [((Constant (Int 1)), (Constant (Int 2)))])) |}]
;;

let%expect_test "Match with two branches" =
  Format.printf "%a" pp (parse_optimistically "match x with 1 -> 2 | 2 -> 3");
  [%expect
    {|
    (Match ((Var "x"),
       [((Constant (Int 1)), (Constant (Int 2)));
         ((Constant (Int 2)), (Constant (Int 3)))]
       )) |}]
;;

let%expect_test "Match with list" =
  Format.printf "%a" pp (parse_optimistically "match x with t::l -> t");
  [%expect {|
    (Match ((Var "x"), [((Cons ((Var "t"), (Var "l"))), (Var "t"))])) |}]
;;

let%expect_test "Match with three branches of three different patterns" =
  Format.printf "%a" pp (parse_optimistically "match x with [] -> 2 | x -> z | t::l -> t");
  [%expect
    {|
    (Match ((Var "x"),
       [((Constant Nil), (Constant (Int 2))); ((Var "x"), (Var "z"));
         ((Cons ((Var "t"), (Var "l"))), (Var "t"))]
       )) |}]
;;

(* Combinated variants *)
let%expect_test "Match nested into if and two lets" =
  Format.printf
    "%a"
    pp
    (parse_optimistically
       "let x = true in\n\
       \                                                 let y = true in\n\
       \                                                   if match x with 1 -> true \
        then 1");
  [%expect
    {|
        (LetIn ((Let (false, "x", None, (Constant (Bool true)))),
           (LetIn ((Let (false, "y", None, (Constant (Bool true)))),
              (IfThenElse (
                 (Match ((Var "x"), [((Constant (Int 1)), (Constant (Bool true)))])),
                 (Constant (Int 1)), (Constant Unit)))
              ))
           )) |}]
;;

let%expect_test "Match nested into lambda let in if" =
  Format.printf
    "%a"
    pp
    (parse_optimistically
       "let lambd = fun x y ->\n\
       \                                                 if x then\n\
       \                                                   y+3\n\
       \                                                 else\n\
       \                                                   y/2+1\n\
       \                                                 in\n\
       \                                                   match x with 2 -> 3+3");
  [%expect
    {|
    (LetIn (
       (Let (false, "lambd", None,
          (Fun ("x",
             (Fun ("y",
                (IfThenElse ((Var "x"),
                   (BinaryOp (Plus, (Var "y"), (Constant (Int 3)))),
                   (BinaryOp (Plus,
                      (BinaryOp (Divide, (Var "y"), (Constant (Int 2)))),
                      (Constant (Int 1))))
                   ))
                ))
             ))
          )),
       (Match ((Var "x"),
          [((Constant (Int 2)),
            (BinaryOp (Plus, (Constant (Int 3)), (Constant (Int 3)))))]
          ))
       )) |}]
;;

(* Typing *)
let%expect_test "ADT declaration of single variant" =
  Format.printf "%a" pp (parse_optimistically "type my=Custom");
  [%expect {| (Type ("my", (AdtT [("Custom", None)]))) |}]
;;

let%expect_test "ADT declaration of several variant" =
  Format.printf "%a" pp (parse_optimistically "type my=First | Second | Third");
  [%expect
    {| (Type ("my", (AdtT [("First", None); ("Second", None); ("Third", None)]))) |}]
;;

let%expect_test "ADT declaration of several variants with types" =
  Format.printf
    "%a"
    pp
    (parse_optimistically "type my=First | Second of int * int | Third of int -> custom");
  [%expect
    {|
    (Type ("my",
       (AdtT
          [("First", None);
            ("Second", (Some (TupleT [(BaseT Int); (BaseT Int)])));
            ("Third", (Some (ArrowT ((BaseT Int), (NamedT ("custom", None))))))])
       )) |}]
;;

let%expect_test "Custom list type" =
  Format.printf "%a" pp (parse_optimistically "type my=int list");
  [%expect {| (Type ("my", (NamedT ("list", (Some (BaseT Int)))))) |}]
;;

let%expect_test "Custom tuple type" =
  Format.printf "%a" pp (parse_optimistically "type my=int * string");
  [%expect {| (Type ("my", (TupleT [(BaseT Int); (BaseT String)]))) |}]
;;

let%expect_test "Custom arrow type" =
  Format.printf "%a" pp (parse_optimistically "type my=int -> string");
  [%expect {| (Type ("my", (ArrowT ((BaseT Int), (BaseT String))))) |}]
;;

let%expect_test "Custom int type" =
  Format.printf "%a" pp (parse_optimistically "type my=int");
  [%expect {| (Type ("my", (BaseT Int))) |}]
;;
