(** Copyright 2021-2022, Artur Gagin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

exception NoSeparator of string (* %% *)

let start_position_of_mly_tokens = 0

(* Position where %% *)
let end_position_of_mly_tokens text =
  try Str.search_forward (Str.regexp "%%") text start_position_of_mly_tokens with
  | Not_found ->
    raise
      (NoSeparator "There is no separator in text (make sure you don't forget symbols %%)")
;;

(* the text where %token and %start only available. *)
let file_text_where_only_tokens_names text =
  String.sub text start_position_of_mly_tokens (end_position_of_mly_tokens text)
;;

let file_text_where_only_rules text =
  String.sub
    text
    (end_position_of_mly_tokens text)
    (String.length text - end_position_of_mly_tokens text)
;;

(*--------------------- elimination of left recursion -----------------------*)
(*
  1. Let's write down all the rules of output from A in the form:
    A -> Aa_1 |...|Aa_n|b_1|...|b_m, where
      > a is a nonempty sequence of terminals and nonterminals (a not -> epsilon);
      > b is a nonempty sequence of terminals and nonterminals not starting with A. 
  2. Replace the rules of inference from A with A -> b_1A'|...|b_mA'|b_1|...|b_m.
  3. Let's create a new nonterminal A'->a_1A'|...|a_nA'|a_1|...|a_n.
*)
let get_h_string_list = function
  | h :: _ -> h
  | _ -> ""
;;

let get_tl_string_list = function
  | _ :: tl -> tl
  | _ -> []
;;

let get_alphas name =
  List.filter_map (fun rule ->
    let lhs, rhs = rule in
    if String.equal lhs name && String.equal (get_h_string_list rhs) name
    then Some (get_tl_string_list rhs)
    else None)
;;

let get_bettas name =
  List.filter_map (fun rule ->
    let lhs, rhs = rule in
    if String.equal lhs name && not (String.equal (get_h_string_list rhs) name)
    then Some rhs
    else None)
;;

let get_new_A_with_A' g a_name =
  let a_name' = a_name ^ "'" in
  let b = get_bettas a_name g in
  let a = get_alphas a_name g in
  let new_a =
    List.map (fun x -> a_name, x @ [ a_name' ]) b @ List.map (fun x -> a_name, x) b
  in
  let a' =
    List.map (fun x -> a_name', x @ [ a_name' ]) a @ List.map (fun x -> a_name', x) a
  in
  new_a @ a'
;;

let rec get_nonterm_names helper = function
  | (lhs, _) :: tl when String.equal lhs helper -> get_nonterm_names helper tl
  | (lhs, _) :: tl -> lhs :: get_nonterm_names lhs tl
  | _ -> []
;;

(* If the rule is not subject to left recursion, we can not fix it to have fewer rules. *)
let is_nonterm_subject_lr name =
  List.exists (fun rule ->
    let lhs, rhs = rule in
    String.equal lhs name && String.equal (get_h_string_list rhs) name)
;;

let lr_grammar_fix g =
  let nonterm_names = get_nonterm_names "" g in
  let rec fix = function
    | h :: tl ->
      if is_nonterm_subject_lr h g
      then get_new_A_with_A' g h @ fix tl
      else
        List.filter
          (fun rule ->
            let lhs, _ = rule in
            String.equal lhs h)
          g
        @ fix tl
    | _ -> []
  in
  fix nonterm_names
;;

(* useless rules: A -> A *)
let delete_useless_rules =
  List.filter (fun rule ->
    let lhs, rhs = rule in
    match rhs with
    | a :: [] when String.equal lhs a -> false
    | _ -> true)
;;

let grammar_fix g = lr_grammar_fix (delete_useless_rules g)

(*---------------------------------------------------------------------------*)

(* tokens, start_rule, grammar *)
let parse' text : string list * string * grammar =
  let parse_tokens_and_start_rule = Lexer.from_string Parser.token_and_start in
  let tokens_and_start_rule =
    parse_tokens_and_start_rule (file_text_where_only_tokens_names text)
  in
  let parse_rules = Lexer.from_string Parser.grammar in
  let grammar = parse_rules (file_text_where_only_rules text) in
  let tokens, start_rule = tokens_and_start_rule in
  tokens, start_rule, (start_rule, grammar_fix grammar)
;;

let read_all_file_text file_path =
  Stdio.In_channel.input_all (Unix.in_channel_of_descr file_path)
;;

let split_string_on_spaces command =
  List.filter
    (fun x -> not (String.equal x " " || String.equal x ""))
    (String.split_on_char ' ' command)
;;

open Stdlib

(*
  We will give comments about what is happening right here on next example:
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

let get_nonterminals (g : grammar) =
  let _, rules = g in
  List.map (fun (nonterm, _) -> nonterm) rules
;;

let rec start_rule_components text start_rule = function
  | (lhs, rhs) :: tl ->
    if String.equal start_rule lhs
    then (lhs, rhs) :: start_rule_components text start_rule tl
    else start_rule_components text start_rule tl
  | [] -> []
;;

let string_list_contains symbol list =
  Base.List.mem list symbol ~equal:(fun x y -> String.equal x y)
;;

(* Берет все правила, которые имеют имя rule_name *)
let get_all_nonterms rule_name grammar =
  let _, rules = grammar in
  List.filter
    (fun rule ->
      let nonterm, _ = rule in
      String.equal nonterm rule_name)
    rules
;;

let list_empty = function
  | [] -> true
  | _ -> false
;;

(* 
  returns true or false depending on whether we can apply the rule, 
  remaining input len if true and error code if false, remaining input 
  if true and empty list if false
*)
let rec try_apply_rule text rule input parse_res =
  let terminals, _, grammar = parse_res in
  let lhs, rhs = rule in
  match rhs with
  | h :: tl ->
    (match input with
     | [] -> false, -1, [] (* OVERSHOOT RIGHT HERE. *)
     | h' :: tl' when string_list_contains h terminals (* TERM SYMBOL *) ->
       if String.equal h' h
       then
         try_apply_rule text (lhs, tl) tl' parse_res
         (* If equal TERM symbols in text and rule then continue checking *)
       else false, 0, [] (* If not equal then false, 0 --- REJECT RIGHT HERE. *)
     | _ :: _ when string_list_contains h (get_nonterminals grammar) (* NONTERM SYMBOL *)
       ->
       (* Get new input if nonterm rule is fits right here. *)
       let rec get_new_input all_nonterms ret =
         match all_nonterms with
         | h' :: tl' ->
           let is_applicable, remaining_input_len, remaining_input =
             try_apply_rule text h' input parse_res
           in
           if is_applicable
           then remaining_input, ret
           else (
             let ret' = if ret = -1 then ret else remaining_input_len in
             get_new_input tl' ret')
         | [] -> input, ret
       in
       let new_input, ret = get_new_input (get_all_nonterms h grammar) 0 in
       if List.compare_lengths new_input input = 0
       then false, ret, []
       else try_apply_rule text (lhs, tl) new_input parse_res
     | _ -> false, 0, [] (* REJECT *))
  | [] -> true, List.length input, input (* remaining input len *)
;;

exception NeverHappenError

let rec apply_rule text rule input parse_res =
  let _, _, grammar = parse_res in
  let lhs, rhs = rule in
  match rhs with
  | h :: tl ->
    if not (string_list_contains h (get_nonterminals grammar))
    then Term h :: apply_rule text (lhs, tl) (List.tl input) parse_res
    else (
      let rec apply = function
        | (lh', rh') :: tl' ->
          let is_applicable, _, remaining_input =
            try_apply_rule text (lh', rh') input parse_res
          in
          if is_applicable
          then
            Nonterm (h, apply_rule text (lh', rh') input parse_res)
            :: apply_rule text (lhs, tl) remaining_input parse_res
          else apply tl'
        | _ -> raise NeverHappenError
        (* Never happen because we checked it earlier in try_apply_rule function. *)
      in
      apply (get_all_nonterms h grammar))
  | [] -> []
;;

let parse text parse_res (input : string list) =
  let _, start_rule, g = parse_res in
  let _, all_rules = g in
  let rec apply input = function
    | h :: tl ->
      if let is_applicable, applied_rule_len, _ = try_apply_rule text h input parse_res in
         is_applicable && applied_rule_len = 0
      then apply_rule text h input parse_res
      else apply input tl
    | [] -> raise NeverHappenError
    (* Never happen because we checked it earlier. *)
  in
  apply input (start_rule_components text start_rule all_rules)
;;

let parse_tree text parse_res (input : string list) =
  let _, start_rule, _ = parse_res in
  let tree_list = parse text parse_res input in
  let main_tree = Nonterm (start_rule, tree_list) in
  let rec print_tree = function
    | Term s -> Format.sprintf " %s " s
    | Nonterm (s, parse_tree_list) ->
      Format.sprintf
        " [ %s : %s ] "
        s
        (String.concat " " (List.map (fun x -> print_tree x) parse_tree_list))
  in
  print_tree main_tree
;;

let try_apply_start_nonterm text parse_res input =
  let _, start_rule, grammar = parse_res in
  let rec applier return_code = function
    | h :: tl ->
      let cond, ret, _ = try_apply_rule text h input parse_res in
      if cond
      then if ret = 0 then true, ret else applier ret tl
      else if return_code = -1
      then applier return_code tl
      else applier ret tl
    | [] -> false, return_code
  in
  applier 0 (get_all_nonterms start_rule grammar)
;;

let gen_parser = try_apply_start_nonterm
let gen_tree_parser = parse_tree

open Lexer

let get_parser_and_tree_parser text =
  (* tokens, start rule, grammar *)
  let parse_result = parse' text in
  gen_parser text parse_result, gen_tree_parser text parse_result
;;

(* TESTS *)
let test_text =
  {|%token INT
   %token PLUS
   %token MUL
   %token LBRACE
   %token RBRACE
   %token EOL
   %start main
   %%
   main:
   | expr; EOL
   | EOL
   expr:
   | LBRACE; expr; RBRACE
   | PLUS; expr; expr
   | MUL; expr; expr
   | INT|}
;;

let%test _ =
  let parser, _ = get_parser_and_tree_parser test_text in
  let res, _ = parser [ "PLUS"; "INT"; "INT"; "EOL" ] in
  res
;;

let%test _ =
  let _, tree_parser = get_parser_and_tree_parser test_text in
  tree_parser [ "PLUS"; "INT"; "INT"; "EOL" ]
  = " [ main :  [ expr :  PLUS   [ expr :  INT  ]   [ expr :  INT  ]  ]   EOL  ] "
;;

let%test _ =
  let parser, _ = get_parser_and_tree_parser test_text in
  let res, _ = parser [ "EOL" ] in
  res
;;

let%test _ =
  let _, tree_parser = get_parser_and_tree_parser test_text in
  tree_parser [ "EOL" ] = " [ main :  EOL  ] "
;;

let%test _ =
  let parser, _ = get_parser_and_tree_parser test_text in
  let res, _ = parser [ "PLUS"; "INT"; "INT" ] in
  not res (* OVERSHOOT *)
;;

let%test _ =
  let parser, _ = get_parser_and_tree_parser test_text in
  let res, _ = parser [ "HELLOWORLD" ] in
  not res (* REJECT *)
;;

let%test _ =
  let parser, _ = get_parser_and_tree_parser test_text in
  let res, _ = parser [ "LBRACE"; "PLUS"; "INT"; "MUL"; "INT"; "INT"; "RBRACE"; "EOL" ] in
  res
;;

let%test _ =
  let _, tree_parser = get_parser_and_tree_parser test_text in
  tree_parser [ "LBRACE"; "PLUS"; "INT"; "MUL"; "INT"; "INT"; "RBRACE"; "EOL" ]
  = " [ main :  [ expr :  LBRACE   [ expr :  PLUS   [ expr :  INT  ]   [ expr :  MUL   [ \
     expr :  INT  ]   [ expr :  INT  ]  ]  ]   RBRACE  ]   EOL  ] "
;;

let%test _ =
  let parser, _ = get_parser_and_tree_parser test_text in
  let res, return_code =
    parser [ "LBRACE"; "PLUS"; "INT"; "MUL"; "INT"; "INT"; "RBRACE"; "EOL"; "EOL" ]
  in
  (not res) && return_code = 0 (* REJECT *)
;;

let test_text = "%token PLU#!@#!KLS"

let%test _ =
  try
    let _ = get_parser_and_tree_parser test_text in
    false
  with
  | NoSeparator _ -> true (* Not found %% *)
  | _ -> false
;;

let test_text = "%token PLU#!@#!KLS %%"

let%test _ =
  try
    let _ = get_parser_and_tree_parser test_text in
    false
  with
  | InvalidToken (_, s) -> String.equal s "#!@#!KLS"
  | _ -> false
;;

(* Lexer error *)

let test_text =
  {|%token WINNIE
   %token PIGLET
   %token TIGER
   %token RABBIT
   %token DONKEY
   %token KANGAROO
   %token MOOMINTROLL
   %token MOOMINMAMMA
   %token MOOMINPAPPA
   %token SNIFF
   %token SNUFKIN
   %token LITTLE_MY
   %token SNORK_MAIDEN
   %token SNORK
   %token EOL
   %start accepted_if_cartoons_same
   %%
   accepted_if_cartoons_same:
   | winnie; EOL
   | moomintroll; EOL
   winnie:
   | WINNIE; winnie
   | WINNIE
   | PIGLET; winnie
   | PIGLET
   | TIGER; winnie
   | TIGER
   | RABBIT; winnie
   | RABBIT
   | DONKEY; winnie
   | DONKEY
   | KANGAROO; winnie
   | KANGAROO
   moomintroll:
   | MOOMINTROLL; moomintroll
   | MOOMINTROLL
   | MOOMINMAMMA; moomintroll
   | MOOMINMAMMA
   | MOOMINPAPPA; moomintroll
   | MOOMINPAPPA
   | SNIFF; moomintroll
   | SNIFF
   | SNUFKIN; moomintroll
   | SNUFKIN
   | LITTLE_MY; moomintroll
   | LITTLE_MY
   | SNORK_MAIDEN; moomintroll
   | SNORK_MAIDEN
   | SNORK; moomintroll
   | SNORK|}
;;

let%test _ =
  let parser, _ = get_parser_and_tree_parser test_text in
  let res, _ = parser [ "WINNIE"; "TIGER"; "RABBIT"; "DONKEY"; "EOL" ] in
  res (* ACCEPT *)
;;

let%test _ =
  let parser, _ = get_parser_and_tree_parser test_text in
  let res, ret = parser [ "WINNIE"; "TIGER"; "RABBIT"; "DONKEY" ] in
  (not res) && ret = -1 (* OVERSHOOT *)
;;

let%test _ =
  let parser, _ = get_parser_and_tree_parser test_text in
  let res, ret = parser [ "WINNIE"; "TIGER"; "RABBIT"; "DONKEY"; "EOL"; "EOL" ] in
  (not res) && ret = 0 (* REJECT *)
;;

let%test _ =
  let parser, _ = get_parser_and_tree_parser test_text in
  let res, ret = parser [ "WINNIE"; "TIGER"; "RABBIT"; "DONKEY"; "QWERTY" ] in
  (not res) && ret = 0 (* REJECT *)
;;

let%test _ =
  let parser, _ = get_parser_and_tree_parser test_text in
  let res, ret = parser [ "WINNIE"; "TIGER"; "RABBIT"; "LITTLE_MY"; "EOL" ] in
  (not res) && ret = 0 (* REJECT *)
;;

let%test _ =
  let parser, _ = get_parser_and_tree_parser test_text in
  let res, ret = parser [ "MOOMINTROLL"; "TIGER"; "MOOMINPAPPA"; "LITTLE_MY"; "EOL" ] in
  (not res) && ret = 0 (* REJECT *)
;;

let%test _ =
  let parser, _ = get_parser_and_tree_parser test_text in
  let res, _ = parser [ "MOOMINTROLL"; "SNORK"; "MOOMINPAPPA"; "LITTLE_MY"; "EOL" ] in
  res (* ACCEPT *)
;;

(* TESTS WITH GRAMMAR SUBJECT TO LEFT RECURSION *)

let test_text =
  {|%token INT
%token PLUS
%token MUL
%token LBRACE
%token RBRACE
%token EOL
%start main
%%
main:
| expr; EOL
| EOL
expr:
| LBRACE; expr; RBRACE
| expr; PLUS; expr
| expr; MUL; expr
| INT|}
;;

let%test _ =
  let parser, _ = get_parser_and_tree_parser test_text in
  let res, ret = parser [ "INT" ] in
  (not res) && ret = -1 (* OVERSHOOT *)
;;

let%test _ =
  let parser, _ = get_parser_and_tree_parser test_text in
  let res, _ = parser [ "INT"; "EOL" ] in
  res (* ACCEPT *)
;;

let%test _ =
  let parser, _ = get_parser_and_tree_parser test_text in
  let res, _ = parser [ "INT"; "PLUS"; "INT"; "EOL" ] in
  res (* ACCEPT *)
;;

let%test _ =
  let parser, _ = get_parser_and_tree_parser test_text in
  let res, _ = parser [ "INT"; "MUL"; "INT"; "PLUS"; "INT"; "EOL" ] in
  res (* ACCEPT *)
;;

let%test _ =
  let parser, _ = get_parser_and_tree_parser test_text in
  let res, _ = parser [ "LBRACE"; "INT"; "PLUS"; "INT"; "MUL"; "INT"; "RBRACE"; "EOL" ] in
  res (* ACCEPT *)
;;

let%test _ =
  let parser, _ = get_parser_and_tree_parser test_text in
  let res, ret = parser [ "RBRACE" ] in
  (not res) && ret = 0 (* REJECT *)
;;

let test_text = {|
%token EOL
%token X
%start main
%%
main:
| main; EOL
| main; EOL
| X
|}

let%test _ =
  let parser, _ = get_parser_and_tree_parser test_text in
  let res, ret = parser [ "Y" ] in
  (not res) && ret = 0 (* REJECT *)
;;

let%test _ =
  let parser, _ = get_parser_and_tree_parser test_text in
  let res, _ = parser [ "X" ] in
  res (* ACCEPT *)
;;

let%test _ =
  let parser, _ = get_parser_and_tree_parser test_text in
  let res, ret = parser [ "EOL" ] in
  (not res) && ret = 0 (* REJECT *)
;;

let%test _ =
  let parser, _ = get_parser_and_tree_parser test_text in
  let res, _ = parser [ "X"; "EOL" ] in
  res (* ACCEPT *)
;;

let%test _ =
  let parser, _ = get_parser_and_tree_parser test_text in
  let res, _ = parser [ "X"; "EOL"; "EOL" ] in
  res (* ACCEPT *)
;;

let%test _ =
  let parser, _ = get_parser_and_tree_parser test_text in
  let res, _ = parser [ "X"; "EOL"; "EOL"; "EOL" ] in
  res (* ACCEPT *)
;;
