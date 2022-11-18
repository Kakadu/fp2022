(** Copyright 2021-2022, Kalashnikov Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Base

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_lower = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_upper = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_letter ch = is_lower ch || is_upper ch

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_wild = function
  | '_' -> true
  | _ -> false
;;

let is_keyword = function
  | "and"
  | "as"
  | "assert"
  | "asr"
  | "begin"
  | "class"
  | "constraint"
  | "do"
  | "done"
  | "downto"
  | "else"
  | "end"
  | "exception"
  | "external"
  | "false"
  | "for"
  | "fun"
  | "function"
  | "functor"
  | "if"
  | "in"
  | "include"
  | "inherit"
  | "initializer"
  | "land"
  | "lazy"
  | "let"
  | "lor"
  | "lsl"
  | "lsr"
  | "lxor"
  | "match"
  | "method"
  | "mod"
  | "module"
  | "mutable"
  | "new"
  | "nonrec"
  | "object"
  | "of"
  | "open"
  | "or"
  | "private"
  | "rec"
  | "sig"
  | "struct"
  | "then"
  | "to"
  | "true"
  | "try"
  | "type"
  | "val"
  | "virtual"
  | "when"
  | "while"
  | "with" -> true
  | _ -> false
;;

(*Plain parsers*)

let empty = take_while is_space
let empty1 = take_while1 is_space
let token s = empty *> string s
let token1 s = empty1 *> string s
let trim p = empty *> p <* empty
let parens p = token "(" *> p <* token ")"
let c_int n = CInt n
let c_string s = CString s
let c_bool b = CBool b
let econst const = EConst const
let ebinopr bin_operator expr1 expr2 = EBinop (bin_operator, expr1, expr2)
let evar name = EVar name
let eif expr1 expr2 expr3 = EIf (expr1, expr2, expr3)
let elet binding expr = ELet (binding, expr)
let efunction cases = EFun (PVar "match", EMatch (EVar "match", cases))
let efun name expr = EFun (PVar name, expr)
let efun args rhs = List.fold_right args ~f:efun ~init:rhs
let ematch expr cases = EMatch (expr, cases)
let eobj exprl = EObj exprl
let ometh pattern expr = OMeth (pattern, expr)
let oval pattern expr = OVal (pattern, expr)
let ecallmeth (name1, name2) = ECallM (name1, name2)
let eapp fn arg = EApp (fn, arg)
let pvar name = PVar name
let pconst c = PConst c

let choice_op ops =
  choice @@ List.map ~f:(fun (tok, cons) -> token tok *> (return @@ ebinopr cons)) ops
;;

let add_sub = choice_op [ "+", Add; "-", Sub ]
let mult_div = choice_op [ "*", Mul; "/", Div ]
let cmp = choice_op [ ">=", Geq; ">", Gre; "<=", Leq; "<", Less ]
let eq_uneq = choice_op [ "=", Eq; "<>", Neq ]
let conj = choice_op [ "&&", And ]
let disj = choice_op [ "||", Or ]
let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

let name =
  empty
  *> lift2
       (fun c oth -> Char.to_string c ^ oth)
       (satisfy (fun c -> is_lower c || is_wild c))
       (take_while (fun c -> is_letter c || is_digit c || is_wild c))
  >>= fun str ->
  if is_keyword str || str == "" || str == "_"
  then fail "Wrong method call"
  else return str
;;

(*-------------- Const parsing --------------*)

let sign =
  choice [ token "+" *> return 1; token "-" *> return (-1); token "" *> return 1 ]
;;

let ucint = Base.Int.of_string <$> empty *> take_while1 is_digit >>| c_int

let cint =
  lift2
    (fun sign uns -> c_int (sign * uns))
    sign
    (take_while1 is_digit
    >>= fun str ->
    match int_of_string_opt str with
    | Some x -> return x
    | None -> fail "int error")
;;

let cbool =
  let _true = token "true" *> return (c_bool true) in
  let _false = token "false" *> return (c_bool false) in
  _true <|> _false
;;

let cstring =
  c_string <$> empty *> (string "\"" *> take_while (fun ch -> ch != '"') <* string "\"")
;;

let const = choice [ cint; cbool; cstring ]
let uconst = choice [ ucint; cbool; cstring ]
let pvar = name >>| pvar
let pconst = const >>| pconst

(*-------------- Expr parsing --------------*)

let prs_decl prs_expr =
  trim
  @@ lift3
       (fun is_rec pvar expr -> is_rec, pvar, expr)
       (token "let" *> option false (token "rec" >>| fun _ -> true))
       (empty1 *> pvar)
       (lift2 efun (empty *> many name <* token "=") prs_expr)
;;

let prs_let prs_expr = lift2 elet (prs_decl prs_expr) (token "in" *> empty1 *> prs_expr)

let prs_efun prs_expr =
  token "fun" *> lift2 efun (empty *> many name <* token "->") prs_expr
;;

let prs_if prs_expr =
  lift3 eif (token "if" *> prs_expr) (token "then" *> prs_expr) (token "else" *> prs_expr)
;;

let prs_match prs_expr =
  let ptrn =
    fix
    @@ fun pttrn ->
    choice [ pconst; pvar; token "(" *> pttrn <* token ")" ]
    <|> token "(" *> token ")" *> return PUnit
  in
  let prs_case =
    lift2 (fun pattern expr -> pattern, expr) (token "|" *> ptrn <* token "->") prs_expr
  in
  lift2 ematch (token "match" *> empty *> prs_expr <* token "with") (many1 prs_case)
;;

let prs_obj prs_expr =
  let omethod =
    trim
    @@ lift2
         ometh
         (token "method" *> pvar)
         (lift2 efun (empty *> many name <* token "=") prs_expr)
  in
  let oval = trim @@ lift2 oval (token "val" *> pvar) (token "=" *> prs_expr) in
  token "object" *> many (choice [ oval; omethod ]) <* token "end" >>| eobj
;;

let prs_call_meth =
  lift2 (fun name1 name2 -> ecallmeth (name1, name2)) (name <* token "#") name
;;

let prs_expr =
  fix (fun pexp ->
    let term =
      choice
        [ token "()" *> return EUnit
        ; parens pexp
        ; econst <$> uconst
        ; prs_call_meth
        ; evar <$> name
        ]
    in
    let term =
      lift2
        (fun expr l -> List.fold_left ~f:eapp ~init:expr l)
        term
        (many (empty *> term))
    in
    let pmultdiv = chainl1 term mult_div in
    let paddsub = chainl1 pmultdiv add_sub in
    let pcmp = chainl1 paddsub cmp in
    let peq = chainl1 pcmp eq_uneq in
    let pconj = chainr1 peq conj in
    choice
      [ prs_obj pexp
      ; prs_let pexp
      ; prs_if pexp
      ; prs_match pexp
      ; prs_efun pexp
      ; empty *> chainr1 pconj disj <* empty
      ])
;;

(*-------------- Prog parsing --------------*)

let getdeclaration a = DLet a
let decl = getdeclaration <$> empty *> prs_decl prs_expr
let pprog (l : declaration list) : prog = l
let prog = sep_by1 (token ";;") decl <* option "" @@ trim (token ";;") >>| pprog
let parse s = parse_string ~consume:All prog s
