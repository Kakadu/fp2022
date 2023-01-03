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

let get_nonterm_names =
  List.fold_left (fun (helper, res) (lhs, _) ->
    if String.equal lhs helper then helper, res else lhs, res @ [ lhs ])
;;

let get_nonterm_names g =
  let _, res = get_nonterm_names ("", []) g in
  res
;;

(* If the rule is not subject to left recursion, we can not fix it to have fewer rules. *)
let is_nonterm_subject_lr name =
  List.exists (fun rule ->
    let lhs, rhs = rule in
    String.equal lhs name && String.equal (get_h_string_list rhs) name)
;;

let lr_grammar_fix g =
  let nonterm_names = get_nonterm_names g in
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

let read_all_file_text file_path = Stdio.In_channel.read_all file_path

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

exception RejectApplyingRule
exception OvershootApplyingRule

(*  
If we got one overshoot error, then we should save this result,
because there is no reject error. 
So, saved_error is_overshoot boolean flag helps with it.
*)
let raise_applying_rule is_overshoot =
  if is_overshoot then raise OvershootApplyingRule else raise RejectApplyingRule
;;

let apply_rule rule input parse_res =
  let terminals, _, grammar = parse_res in
  let nonterminals = get_nonterminals grammar in
  let rec apply_rule rule input terminals nonterminals grammar =
    let lhs, rhs = rule in
    match rhs with
    | h :: tl ->
      (match input with
       | [] -> raise OvershootApplyingRule (* OVERSHOOT RIGHT HERE. *)
       | h' :: tl' when string_list_contains h terminals (* TERM SYMBOL *) ->
         if String.equal h' h
         then (
           let res, remaining_input =
             apply_rule (lhs, tl) tl' terminals nonterminals grammar
           in
           Term h :: res, remaining_input
           (* If equal TERM symbols in text and rule then continue checking *))
         else raise RejectApplyingRule (* REJECT. *)
       | _ :: _ when string_list_contains h nonterminals (* NONTERM SYMBOL *) ->
         (* Get new input if nonterm rule is fits right here. *)
         let rec try_apply_nonterm all_nonterms is_overshoot =
           match all_nonterms with
           | h' :: tl' ->
             let lhs', _ = h' in
             (try
                let result, remaining_input =
                  apply_rule h' input terminals nonterminals grammar
                in
                let tl_result', tl_remaining_input' =
                  apply_rule (lhs', tl) remaining_input terminals nonterminals grammar
                in
                Nonterm (lhs', result) :: tl_result', tl_remaining_input'
              with
              | OvershootApplyingRule -> try_apply_nonterm tl' true
              | RejectApplyingRule -> try_apply_nonterm tl' is_overshoot)
           | [] -> raise_applying_rule is_overshoot
         in
         try_apply_nonterm (get_all_nonterms h grammar) false
       | _ -> raise RejectApplyingRule (* REJECT *))
    | [] -> [], input
  in
  apply_rule rule input terminals nonterminals grammar
;;

let apply_rule rule input parse_res =
  let result, remaining_input = apply_rule rule input parse_res in
  if list_empty remaining_input then result else raise RejectApplyingRule
;;

(* parse_tree list to string *)
let parse_tree parse_res tree =
  let _, start_rule, _ = parse_res in
  let main_tree = Nonterm (start_rule, tree) in
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

let try_apply_start_nonterm parse_res input =
  let _, start_rule, grammar = parse_res in
  let rec applier is_overshoot = function
    | h :: tl ->
      (try apply_rule h input parse_res with
       | OvershootApplyingRule -> applier true tl
       | RejectApplyingRule -> applier is_overshoot tl)
    | [] -> raise_applying_rule is_overshoot
  in
  applier false (get_all_nonterms start_rule grammar)
;;

let gen_parser = try_apply_start_nonterm
let gen_tree_printer = parse_tree

open Lexer

let get_parser_and_tree_printer text =
  (* tokens, start rule, grammar *)
  let parse_result = parse' text in
  gen_parser parse_result, gen_tree_printer parse_result
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

let get_expected_tree token_list =
  let parser, tree_printer = get_parser_and_tree_printer test_text in
  tree_printer (parser token_list)
;;

let%test _ =
  get_expected_tree [ "PLUS"; "INT"; "INT"; "EOL" ]
  = " [ main :  [ expr :  PLUS   [ expr :  INT  ]   [ expr :  INT  ]  ]   EOL  ] "
;;

let%test _ = get_expected_tree [ "EOL" ] = " [ main :  EOL  ] "

let%test _ =
  try get_expected_tree [ "PLUS"; "INT"; "INT" ] = "" with
  | OvershootApplyingRule -> true
  | _ -> false
;;

let%test _ =
  try get_expected_tree [ "HELLOWORLD" ] = "" with
  | RejectApplyingRule -> true
  | _ -> false
;;

let%test _ =
  get_expected_tree [ "LBRACE"; "PLUS"; "INT"; "MUL"; "INT"; "INT"; "RBRACE"; "EOL" ]
  = " [ main :  [ expr :  LBRACE   [ expr :  PLUS   [ expr :  INT  ]   [ expr :  MUL   [ \
     expr :  INT  ]   [ expr :  INT  ]  ]  ]   RBRACE  ]   EOL  ] "
;;

let%test _ =
  try
    get_expected_tree
      [ "LBRACE"; "PLUS"; "INT"; "MUL"; "INT"; "INT"; "RBRACE"; "EOL"; "EOL" ]
    = ""
  with
  | RejectApplyingRule -> true
  | _ -> false
;;

let test_text = "%token PLU#!@#!KLS"

let%test _ =
  try
    let _ = get_parser_and_tree_printer test_text in
    false
  with
  | NoSeparator _ -> true (* Not found %% *)
  | _ -> false
;;

let test_text = "%token PLU#!@#!KLS %%"

let%test _ =
  try
    let _ = get_parser_and_tree_printer test_text in
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

let get_expected_tree token_list =
  let parser, tree_printer = get_parser_and_tree_printer test_text in
  tree_printer (parser token_list)
;;

let%test _ =
  get_expected_tree [ "WINNIE"; "TIGER"; "RABBIT"; "DONKEY"; "EOL" ]
  = " [ accepted_if_cartoons_same :  [ winnie :  WINNIE   [ winnie :  TIGER   [ winnie \
     :  RABBIT   [ winnie :  DONKEY  ]  ]  ]  ]   EOL  ] "
;;

let%test _ =
  try get_expected_tree [ "WINNIE"; "TIGER"; "RABBIT"; "DONKEY" ] = "" with
  | OvershootApplyingRule -> true
  | _ -> false
;;

let%test _ =
  try get_expected_tree [ "WINNIE"; "TIGER"; "RABBIT"; "DONKEY"; "EOL"; "EOL" ] = "" with
  | RejectApplyingRule -> true
  | _ -> false
;;

let%test _ =
  try get_expected_tree [ "WINNIE"; "TIGER"; "RABBIT"; "DONKEY"; "QWERTY" ] = "" with
  | RejectApplyingRule -> true
  | _ -> false
;;

let%test _ =
  try get_expected_tree [ "WINNIE"; "TIGER"; "RABBIT"; "LITTLE_MY"; "EOL" ] = "" with
  | RejectApplyingRule -> true
  | _ -> false
;;

let%test _ =
  try
    get_expected_tree [ "MOOMINTROLL"; "TIGER"; "MOOMINPAPPA"; "LITTLE_MY"; "EOL" ] = ""
  with
  | RejectApplyingRule -> true
  | _ -> false
;;

let%test _ =
  get_expected_tree [ "MOOMINTROLL"; "SNORK"; "MOOMINPAPPA"; "LITTLE_MY"; "EOL" ]
  = " [ accepted_if_cartoons_same :  [ moomintroll :  MOOMINTROLL   [ moomintroll :  \
     SNORK   [ moomintroll :  MOOMINPAPPA   [ moomintroll :  LITTLE_MY  ]  ]  ]  ]   \
     EOL  ] "
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

let get_expected_tree token_list =
  let parser, tree_printer = get_parser_and_tree_printer test_text in
  tree_printer (parser token_list)
;;

let%test _ =
  try get_expected_tree [ "INT" ] = "" with
  | OvershootApplyingRule -> true
  | _ -> false
;;

let%test _ = get_expected_tree [ "INT"; "EOL" ] = " [ main :  [ expr :  INT  ]   EOL  ] "

let%test _ =
  get_expected_tree [ "INT"; "PLUS"; "INT"; "EOL" ]
  = " [ main :  [ expr :  INT   [ expr' :  PLUS   [ expr :  INT  ]  ]  ]   EOL  ] "
;;

let%test _ =
  get_expected_tree [ "INT"; "MUL"; "INT"; "PLUS"; "INT"; "EOL" ]
  = " [ main :  [ expr :  INT   [ expr' :  MUL   [ expr :  INT  ]   [ expr' :  PLUS   [ \
     expr :  INT  ]  ]  ]  ]   EOL  ] "
;;

let%test _ =
  get_expected_tree [ "LBRACE"; "INT"; "PLUS"; "INT"; "MUL"; "INT"; "RBRACE"; "EOL" ]
  = " [ main :  [ expr :  LBRACE   [ expr :  INT   [ expr' :  PLUS   [ expr :  INT  ]   \
     [ expr' :  MUL   [ expr :  INT  ]  ]  ]  ]   RBRACE  ]   EOL  ] "
;;

let%test _ =
  try get_expected_tree [ "RBRACE" ] = "" with
  | RejectApplyingRule -> true
  | _ -> false
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

let get_expected_tree token_list =
  let parser, tree_printer = get_parser_and_tree_printer test_text in
  tree_printer (parser token_list)
;;

let%test _ =
  try get_expected_tree [ "Y" ] = "" with
  | RejectApplyingRule -> true
  | _ -> false
;;

let%test _ = get_expected_tree [ "X" ] = " [ main :  X  ] "

let%test _ =
  try get_expected_tree [ "EOL" ] = "" with
  | RejectApplyingRule -> true
  | _ -> false
;;

let%test _ = get_expected_tree [ "X"; "EOL" ] = " [ main :  X   [ main' :  EOL  ]  ] "

let%test _ =
  get_expected_tree [ "X"; "EOL"; "EOL" ]
  = " [ main :  X   [ main' :  EOL   [ main' :  EOL  ]  ]  ] "
;;

let%test _ =
  get_expected_tree [ "X"; "EOL"; "EOL"; "EOL" ]
  = " [ main :  X   [ main' :  EOL   [ main' :  EOL   [ main' :  EOL  ]  ]  ]  ] "
;;
