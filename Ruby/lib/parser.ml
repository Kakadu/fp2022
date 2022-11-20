(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Lexer
open Ast

let bool_t = true_t <|> false_t

let literal =
  choice
    [ (integer_t >>| fun s -> IntegerL, s)
    ; (bool_t >>| fun s -> BoolL, s)
    ; (ruby_string >>| fun s -> StringL, s)
    ]
  >>| fun (t, s) -> Literal (t, s)
;;

let wr_binop p = p >>| fun op l r -> Binop (op, l, r)
let binop_v = wr_binop binops
let asoc0 = wr_binop asoc0_t
let asoc1 = wr_binop asoc1_t
let asoc2 = wr_binop asoc2_t
let var_cal = identifier_t >>| fun s -> Var s

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let parens p = token "(" *> p <* token ")"
let maybe p = p <|> return ""

let seq_of_expr =
  fix (fun seq_of_expr ->
    let expr =
      fix (fun expr ->
        (* --- Functions --- *)
        let parameters_decl = token "(" *> sep_by (token ",") identifier_t <* token ")" in
        let function_declaration =
          token "def" *> identifier_t
          >>= fun func_name ->
          option [] parameters_decl
          >>= fun params ->
          maybe new_lines *> seq_of_expr
          <* maybe new_lines
          <* token "end"
          >>| fun f_body -> FuncDeclaration (func_name, params, f_body)
        in
        let invocation =
          choice [ var_cal; parens expr ]
          >>= fun inv_box ->
          token "(" *> sep_by (token ",") expr
          <* token ")"
          >>| fun param_values -> Invocation (inv_box, param_values)
        in
        (* --- Var assn --- *)
        let assn =
          identifier_t
          >>= fun i -> token "=" *> expr >>| fun var_val -> VarAssign (i, var_val)
        in
        (* --- While --- *)
        let while_loop =
          token "while" *> expr
          >>= fun cond ->
          maybe (token "do") *> seq_of_expr
          <* token "end"
          >>= fun body -> return (WhileLoop (cond, body))
        in
        (* --- Conditional ---*)
        let conditional =
          token "if" *> maybe new_lines *> expr
          <* maybe new_lines
          >>= (fun condition ->
                token "then" *> maybe new_lines *> seq_of_expr
                <* maybe new_lines
                >>= fun thenB ->
                option
                  (Conditional (condition, thenB, Literal (NilL, "Nil")))
                  (string "else" *> maybe new_lines *> seq_of_expr
                  <* maybe new_lines
                  >>= fun elseB -> return (Conditional (condition, thenB, elseB))))
          <* token "end"
        in
        (* --- Array declaration --- *)
        let array_t = token "[" *> sep_by (token ",") expr <* token "]" in
        let array_v = array_t >>| fun arr -> ArrayDecl arr in
        (* --- Indexing --- *)
        let index_p =
          choice
            ~failure_msg:"Unrecognized index target"
            [ var_cal
            ; (ruby_string >>| fun s -> Literal (StringL, s))
            ; array_v
            ; parens expr
            ]
          >>= fun box -> token "[" *> expr <* token "]" >>| fun ind -> Indexing (box, ind)
        in
        (* --- Binops ---*)
        let factor =
          choice
            ~failure_msg:"Unrecognized factor"
            [ index_p; invocation; parens expr; literal; var_cal; conditional; array_v ]
        in
        let asoc0_p = chainl1 factor asoc0 in
        let asoc1_p = chainl1 asoc0_p asoc1 in
        let asoc2_p = chainl1 asoc1_p asoc2 in
        (* --- Expr definition --- *)
        choice
          ~failure_msg:"Unrecognized expression"
          [ function_declaration; assn; asoc2_p; parens expr; while_loop; array_v ])
    in
    maybe new_lines *> sep_by expr_separator expr <* maybe new_lines >>| fun s -> Seq s)
;;

let parse (str : string) : ast =
  match parse_string ~consume:All seq_of_expr str with
  | Ok v -> v
  | Error msg -> failwith msg
;;
