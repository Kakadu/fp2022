open Base
open Ast

(* LEXICAL ANALYSIS *)

(*------------------------REGEXP------------------------*)
let regexp_whitespace = Str.regexp "[\n\t\r ]"
let regexp_procentprocent = Str.regexp "%%"
let regexp_token = Str.regexp "%token[ \n\t\r]+[A-Z]+"
let regexp_start = Str.regexp "%start\n*[ \n\t\r]+[a-z]+"

(*   "\\(     [ \n\t\r]+     |     \\(     [ \n\t\r]+     [a-zA-Z]+;     \\)+     \\)+"   *)
let regexp_rule = Str.regexp "[a-z]+:\\([ \n\t\r]+|\\([ \t\r]+[a-zA-Z]+[;]?\\)+\\)+"

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

exception ParseError of string

(* Если в строке осталось что-то, кроме %token и %start, то выдаем ошибку. *)
(* Замечание: если %start присутствует в тексте более, чем один раз, то это тоже ParseError. *)
let check_string_on_errors str =
  let probably_empty_string = Str.global_replace regexp_whitespace "" !str in
  if String.equal probably_empty_string ""
  then true
  else
    raise (ParseError ("There are symbols which I can't parse: " ^ probably_empty_string))
;;

exception NoStartToken of string

(* Берем %start. *)
let get_start_rule str =
  if is_string_contains regexp_start !str >= 0
  then (
    let start = Str.matched_string !str in
    str := Str.replace_first regexp_start "" !str;
    start, str)
  else raise (NoStartToken "No %start in input file")
;;

(* Берем токены из строки и удаляем считанные. *)
let rec get_tokens_list str =
  if is_string_contains regexp_token !str >= 0
  then (
    let tkn = Str.matched_string !str in
    str := Str.replace_first regexp_token "" !str;
    tkn :: get_tokens_list str)
  else if check_string_on_errors str
  then []
  else [] (* Last else ~ failwith in check function *)
;;

(* Берем список токенов и стартовое правило. *)
let start_rule_and_token_list text =
  let s = file_text_where_only_tokens_names text in
  let s, s_ref = get_start_rule s in
  ( Str.replace_first (Str.regexp "%start[\n\t\r ]+") "" s
  , List.map
      ~f:(fun s -> Str.replace_first (Str.regexp "%token[\n\t\r ]+") "" s)
      (get_tokens_list s_ref) )
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
  else if check_string_on_errors str
  then []
  else [] (* Last else ~ failwith in check function *)
;;

(* Takes grammar from text *)
let take_grammar text =
  let start_rule, _ = start_rule_and_token_list text in
  start_rule, get_rules_list (file_text_where_only_rules text)
;;

open Stdlib

let get_nonterminals (g : grammar) =
  let _, rules = g in
  List.map (fun (nonterm, _) -> nonterm) rules
;;

let start_rule text =
  let r, _ = start_rule_and_token_list text in
  r
;;

let token_list text =
  let _, l = start_rule_and_token_list text in
  l
;;

(* TESTS *)

let grammar_compare (g : grammar) (g' : grammar) : bool =
  let start_rule_g, rules_g = g in
  let start_rule_g', rules_g' = g' in
  String.equal start_rule_g start_rule_g'
  && List.equal
       (fun (g_h, g_tl) (g'_h, g'_tl) ->
         String.equal g_h g'_h && List.equal String.equal g_tl g'_tl)
       rules_g
       rules_g'
;;

let string_list_compare n n' = List.equal String.equal n n'

(* TEST 1: EVERYTHING IS OKAY: *)
let test_text =
  "%token INT\n\
   %token PLUS\n\
   %token MUL\n\
   %token LBRACE\n\
   %token RBRACE\n\
   %token EOL\n\
   %start main\n\
   %%\n\
   main:\n\
  \    | expr; EOL\n\
  \    | EOL\n\
   expr:\n\
  \    | LBRACE; expr; RBRACE\n\
  \    | PLUS; expr; expr\n\
  \    | MUL; expr; expr\n\
  \    | INT"
;;

let%test "start_rule_test" = String.equal (start_rule test_text) "main"

let%test "nonterms_test" =
  string_list_compare
    (get_nonterminals (take_grammar test_text))
    [ "main"; "main"; "expr"; "expr"; "expr"; "expr" ]
;;

let%test "grammar_and_terms_test" =
  let x = take_grammar test_text in
  let y = token_list test_text in
  grammar_compare
    x
    ( "main"
    , [ "main", [ "expr"; "EOL" ]
      ; "main", [ "EOL" ]
      ; "expr", [ "LBRACE"; "expr"; "RBRACE" ]
      ; "expr", [ "PLUS"; "expr"; "expr" ]
      ; "expr", [ "MUL"; "expr"; "expr" ]
      ; "expr", [ "INT" ]
      ] )
  && string_list_compare y [ "INT"; "PLUS"; "MUL"; "LBRACE"; "RBRACE"; "EOL" ]
;;

(* TEST 2: LOWERCASE TOKEN IN TOKENS DESCRIPTION: *)
let test_text =
  "%token INT\n\
   %token plus\n\
   %token MUL\n\
   %token LBRACE\n\
   %token RBRACE\n\
   %token EOL\n\
   %start main\n\
   %%\n\
   main:\n\
  \    | expr; EOL\n\
  \    | EOL\n\
   expr:\n\
  \    | LBRACE; expr; RBRACE\n\
  \    | PLUS; expr; expr\n\
  \    | MUL; expr; expr\n\
  \    | INT"
;;

let%test "tokens_test" =
  try
    string_list_compare
      (token_list test_text)
      [ "INT"; "plus"; "MUL"; "LBRACE"; "RBRACE"; "EOL" ]
  with
  | ParseError s -> s = "There are symbols which I can't parse: %tokenplus"
;;

(* TEST 3: TOKEN WITH BADSYMBOLS IN TOKENS DESCRIPTION: *)
let test_text =
  "%token INT\n\
   %token PLUS\n\
   %token M1_$!@#*&(UL\n\
   %token LBRACE\n\
   %token RBRACE\n\
   %token EOL\n\
   %start main\n\
   %%\n\
   main:\n\
  \    | expr; EOL\n\
  \    | EOL\n\
   expr:\n\
  \    | LBRACE; expr; RBRACE\n\
  \    | PLUS; expr; expr\n\
  \    | MUL; expr; expr\n\
  \    | INT"
;;

let%test "tokens_test" =
  try
    string_list_compare
      (token_list test_text)
      [ "INT"; "plus"; "M1_$!@#*&(UL"; "LBRACE"; "RBRACE"; "EOL" ]
  with
  | ParseError s -> s = "There are symbols which I can't parse: 1_$!@#*&(UL"
;;

(* TEST 4: NO START RULE IN TOKENS DESCRIPTION: *)
let test_text =
  "%token INT\n\
   %token PLUS\n\
   %token MUL\n\
   %token LBRACE\n\
   %token RBRACE\n\
   %token EOL\n\
   %%\n\
   main:\n\
  \    | expr; EOL\n\
  \    | EOL\n\
   expr:\n\
  \    | LBRACE; expr; RBRACE\n\
  \    | PLUS; expr; expr\n\
  \    | MUL; expr; expr\n\
  \    | INT"
;;

let%test "tokens_test" =
  try String.equal (start_rule test_text) "" with
  | NoStartToken s -> s = "No %start in input file"
;;

(* TEST 5: UPPERCASE START TOKEN IN TOKENS DESCRIPTION: *)
let test_text =
  "%token INT\n\
   %token PLUS\n\
   %token MUL\n\
   %token LBRACE\n\
   %token RBRACE\n\
   %token EOL\n\
   %start mAIN\n\
   %%\n\
   main:\n\
  \    | expr; EOL\n\
  \    | EOL\n\
   expr:\n\
  \    | LBRACE; expr; RBRACE\n\
  \    | PLUS; expr; expr\n\
  \    | MUL; expr; expr\n\
  \    | INT"
;;

let%test "tokens_test" =
  try String.equal (start_rule test_text) "" with
  | ParseError s -> s = "There are symbols which I can't parse: AIN"
;;

(* TEST 6: UPPERCASE RULE IN RULE DESCRIPTION: *)
let test_text =
  "%token INT\n\
   %token PLUS\n\
   %token MUL\n\
   %token LBRACE\n\
   %token RBRACE\n\
   %token EOL\n\
   %start main\n\
   %%\n\
   MAIN:\n\
  \    | expr; EOL\n\
  \    | EOL\n\
   expr:\n\
  \    | LBRACE; expr; RBRACE\n\
  \    | PLUS; expr; expr\n\
  \    | MUL; expr; expr\n\
  \    | INT"
;;

let%test "tokens_test" =
  try take_grammar test_text = ("", [ "", [ "" ] ]) with
  | ParseError s -> s = "There are symbols which I can't parse: MAIN:|expr;EOL|EOL"
;;

(*  
Note that something like this:    
  expr:
      | LBRACE; sOmEtHiNg; RBRACE
will be useless rule, but no one forbids user to enter it. 
*)
