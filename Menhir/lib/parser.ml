open Base
open Ast

exception GetPathError of string

(* LEXICAL ANALYSIS *)

(*------------------------REGEXP------------------------*)
let regexp_whitespace = Str.regexp "[\n\t\r ]"
let regexp_procentprocent = Str.regexp "%%"
let regexp_token = Str.regexp "%token[ \n\t\r]+[A-Z]+"
let regexp_start = Str.regexp "%start\n*[ \n\t\r]+[a-zA-Z]+"

(*   "\\(     [ \n\t\r]+     |     \\(     [ \n\t\r]+     [a-zA-Z]+;     \\)+     \\)+"   *)
let regexp_rule = Str.regexp "[a-zA-Z]+:\\([ \n\t\r]+|\\([ \t\r]+[a-zA-Z]+[;]?\\)+\\)+"
let regexp_rule_name = Str.regexp "[ \n\t\r]*[a-zA-Z]+[ \n\t\r]*:"
let regexp_rule_component = Str.regexp "[ \n\t\r]+|\\([ \n\t\r]+[a-zA-Z]+;\\)+"
(*------------------------------------------------------*)

(* Start position *)
let start_position_of_mly_tokens = 0

(* Position where %% *)
let end_position_of_mly_tokens text =
  Str.search_forward regexp_procentprocent text start_position_of_mly_tokens
;;

(* the text where %token and %start only available. *)
let file_text_where_only_tokens_names text =
  ref
    (String.sub
       text
       ~pos:start_position_of_mly_tokens
       ~len:(end_position_of_mly_tokens text))
;;

(* the text where only rules available. *)
let file_text_where_only_rules text =
  ref
    (String.sub
       text
       ~pos:(end_position_of_mly_tokens text + 2)
       ~len:(String.length text - end_position_of_mly_tokens text - 2))
;;

(* Если строка не содержит строку, которую можно найти с помощью переданного регулярного
    выражения, то возвращаем -1. *)
let is_string_contains regexp str =
  try Str.search_forward regexp str 0 with
  | _ -> -1
;;

(* Берем токены из строки и удаляем считанные. *)
let rec get_tokens_list str =
  if is_string_contains regexp_token !str >= 0
  then (
    let tkn = Str.matched_string !str in
    str := Str.replace_first regexp_token "" !str;
    tkn :: get_tokens_list str)
  else []
;;

exception NoStartToken of string

(* Берем %start. *)
let get_start_rule str =
  if is_string_contains regexp_start !str >= 0
  then (
    let start = Str.matched_string !str in
    str := Str.replace_first regexp_start "" !str;
    start)
  else raise (NoStartToken "No %start in input file")
;;

exception ParseError of string

(* Если в строке осталось что-то, кроме %token и %start, то выдаем ошибку. *)
(* Замечание: если %start присутствует в тексте более, чем один раз, то это тоже ParseError. *)
let check_string_with_tokens_on_errors str =
  let probably_empty_string = Str.global_replace regexp_whitespace "" !str in
  if String.equal probably_empty_string ""
  then true
  else
    raise (ParseError ("There are symbols which I can't parse: " ^ probably_empty_string))
;;

(* Берем список токенов. *)
let token_list text =
  List.map
    ~f:(fun s -> Str.replace_first (Str.regexp "%token[\n\t\r ]+") "" s)
    (get_tokens_list (file_text_where_only_tokens_names text))
;;

(* Берем название стартового правила. *)
let start_rule text =
  Str.replace_first
    (Str.regexp "%start[\n\t\r ]+")
    ""
    (get_start_rule (file_text_where_only_tokens_names text))
;;

(* Проверка на ошибки. *)
let do_check_for_parse_errors_tokens text =
  check_string_with_tokens_on_errors (file_text_where_only_tokens_names text)
;;

(* Функция, которая разделяет строку на пару по регулярному выражению. *)
let split_string_on_pair str regexpr =
  let regexpr_pos = Str.search_forward regexpr str 0 in
  let regexpr_len = String.length (Str.matched_string str) in
  ( String.sub str ~pos:0 ~len:regexpr_pos
  , String.sub
      str
      ~pos:(regexpr_pos + regexpr_len)
      ~len:(String.length str - regexpr_pos - regexpr_len) )
;;

(*  Функция для взятия списка пар, где первый элемент является именем правила, а 
      следующий -- списком списков компонентов правил.
    Более понятно на примере:  
    [("main", [["expr"]; ["EOL"]]);
     ("expr", [["LBRACE"; "expr"; "RBRACE"]; ["expr"; "MUL"; "expr"]; ["NUM"]])].
*)
let rec get_rules_list str =
  if is_string_contains regexp_rule !str >= 0
  then (
    let rule = Str.matched_string !str in
    let rule_name, rule_cmpn = split_string_on_pair rule (Str.regexp ":[\n\t\r ]*|") in
    str := Str.replace_first regexp_rule "" !str;
    let l =
      List.map
        ~f:(fun s ->
          ( rule_name
          , String.split ~on:';' (Str.global_replace (Str.regexp "[\n\t\r ]") "" s) ))
        (String.split ~on:'|' rule_cmpn)
    in
    l @ get_rules_list str)
  else []
;;

(* Takes grammar from text *)
let take_grammar text = start_rule text, get_rules_list (file_text_where_only_rules text)

(* Syntax analysis *)

open Stdlib

(*
  We will give comments about what is happeing right here on next example:
    ("main",
      [("main", ["expr"; "EOL"]); ("main", ["EOL"]);
        ("expr", ["LBRACE"; "expr"; "RBRACE"]); ("expr", ["MUL"; "expr"; "expr"]);
        ("expr", ["PLUS"; "expr"; "expr"]); ("expr", ["INT"])])  

  It is like:
    main:
      | expr; EOL
      | EOL
    expr:
      | LBRACE; expr; RBRACE
      | MUL; expr; expr
      | PLUS; expr; expr
      | INT
  
  Note that operator goes first in addition and multiplication in this example: PLUS INT INT and MUL INT INT.
*)

(* Takes start rule (%start) from our grammar. *)
(* From example we take "main". *)
let getStartSymbol (g : grammar) =
  let start, _ = g in
  start
;;

(* Takes all nonterminals symbols. *)
(* From example we take ["main"; "main"; "expr"; "expr"; "expr"; "expr"] *)
let getNonterminals (g : grammar) =
  let _, rules = g in
  List.map (fun (nonterm, _) -> nonterm) rules
;;

let rec get_last_n_elements_from_list n l =
  if List.length l <= n
  then l
  else (
    match l with
    | _ :: tl -> get_last_n_elements_from_list n tl
    | _ -> [])
;;

let getRhs = function
  | _, rhs -> rhs
;;

let nonterminals text = getNonterminals (take_grammar text)

let rec start_rule_components text = function
  | (lhs, rhs) :: tl ->
    if String.equal (start_rule text) lhs
    then (lhs, rhs) :: start_rule_components text tl
    else start_rule_components text tl
  | [] -> []
;;

let start_nonterminals text =
  List.filter (fun s -> String.equal s (start_rule text)) (nonterminals text)
;;

let rec is_symbol_nonterm symbol (g : grammar) nonterminals =
  match nonterminals with
  | h :: tl -> if String.equal h symbol then true else is_symbol_nonterm symbol g tl
  | [] -> false
;;

(* Берет все правила, которые имеют имя rule_name *)
let getAllNonterminalsOfRule rule_name text =
  let _, rules = take_grammar text in
  List.filter
    (fun rule ->
      let nonterm, _ = rule in
      if String.equal nonterm rule_name then true else false)
    rules
;;

(* Здесь мы должны быть уверены, что длина списка input >= длине списка rhs. *)
let rec tryApplyRule text rule input =
  let lhs, rhs = rule in
  match rhs with
  | h :: tl ->
    if List.length input = 0 (* OVERSHOOT RIGHT HERE. *)
    then false, -1
    else if not (is_symbol_nonterm h (take_grammar text) (nonterminals text))
    then
      if String.equal (List.hd input) h
      then tryApplyRule text (lhs, tl) (List.tl input)
      else false, 0
    else (
      let rec getNewInputIfNonterminalRuleIsFits allNonterminalsOfRule return_code =
        match allNonterminalsOfRule with
        | h' :: tl' ->
          let flag, remaining_input_len = tryApplyRule text h' input in
          if flag
          then get_last_n_elements_from_list remaining_input_len input, return_code
          else if return_code = -1
          then getNewInputIfNonterminalRuleIsFits tl' return_code
          else getNewInputIfNonterminalRuleIsFits tl' remaining_input_len
        | [] -> input, return_code
      in
      let new_input, return_code =
        getNewInputIfNonterminalRuleIsFits (getAllNonterminalsOfRule h text) 0
      in
      if List.length new_input = List.length input
      then false, return_code
      else tryApplyRule text (lhs, tl) new_input)
  | [] -> true, List.length input (* remaining input len *)
;;

let rec applyRule text rule input =
  let lhs, rhs = rule in
  match rhs with
  | h :: tl ->
    if not (is_symbol_nonterm h (take_grammar text) (nonterminals text))
    then Term h :: applyRule text (lhs, tl) (List.tl input)
    else (
      let rec x newRules =
        match newRules with
        | (lh', rh') :: tl' ->
          let flag, remaining_input_len = tryApplyRule text (lh', rh') input in
          if flag
          then
            Nonterm (h, applyRule text (lh', rh') input)
            :: applyRule
                 text
                 (lhs, tl)
                 (get_last_n_elements_from_list remaining_input_len input)
          else x tl'
        | _ ->
          failwith
            "Should never happen because we checked it earlier in tryApplyRule function."
      in
      x (getAllNonterminalsOfRule h text))
  | [] -> []
;;

let parse text (g : grammar) (input : string list) =
  let _, allRules = g in
  let rec rulesApplier input rules =
    match rules with
    | h :: tl ->
      if let flag, applied_rule_len = tryApplyRule text h input in
         flag && applied_rule_len = 0
      then applyRule text h input
      else rulesApplier input tl
    | [] -> failwith "No such rule for your input"
  in
  rulesApplier input (start_rule_components text allRules)
;;

let parseTree text (g : grammar) (input : string list) =
  let tree_list = parse text g input in
  let main_tree = Nonterm (start_rule text, tree_list) in
  let rec printTree tree =
    match tree with
    | Term s -> " " ^ s ^ " "
    | Nonterm (s, parseTreeList) ->
      " [ "
      ^ s
      ^ " : "
      ^ String.concat " " (List.map (fun x -> printTree x) parseTreeList)
      ^ " ] "
  in
  printTree main_tree
;;

let tryApplyStartNonterm text input =
  let rec x start_nonterms return_code =
    match start_nonterms with
    | h :: tl ->
      let cond, ret = tryApplyRule text h input in
      if cond
      then if ret = 0 then true, ret else x tl ret
      else if return_code = -1
      then x tl return_code
      else x tl ret
    | [] -> false, return_code
  in
  x (getAllNonterminalsOfRule (start_rule text) text) 0
;;

let genParser text = tryApplyStartNonterm text
let genTreeParser text = parseTree text (take_grammar text)
