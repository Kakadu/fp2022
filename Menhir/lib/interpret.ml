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

(* tokens, start_rule, grammar *)
let parse text =
  let parse_tokens_and_start_rule = Lexer.from_string Parser.token_and_start in
  let tokens_and_start_rule =
    parse_tokens_and_start_rule (file_text_where_only_tokens_names text)
  in
  let parse_rules = Lexer.from_string Parser.grammar in
  let grammar = parse_rules (file_text_where_only_rules text) in
  let tokens, start_rule = tokens_and_start_rule in
  tokens, start_rule, grammar
;;

let take_grammar text : grammar =
  let _, s_r, g = parse text in
  s_r, g
;;

let get_tokens text =
  let t_l, _, _ = parse text in
  t_l
;;

let start_rule text =
  let _, s_r, _ = parse text in
  s_r
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

let get_last_elements_from_list (n : int) l =
  let len = List.length l in
  if len <= n
  then l
  else (
    let rec deleter l counter =
      if counter > 0
      then (
        match l with
        | _ :: tl -> deleter tl (counter - 1)
        | _ -> [])
      else l
    in
    deleter l (len - n))
;;

let getRhs = function
  | _, rhs -> rhs
;;

let get_nonterminals (g : grammar) =
  let _, rules = g in
  List.map (fun (nonterm, _) -> nonterm) rules
;;

let nonterminals text = get_nonterminals (take_grammar text)
let terminals text = get_tokens text

let rec start_rule_components text = function
  | (lhs, rhs) :: tl ->
    if String.equal (start_rule text) lhs
    then (lhs, rhs) :: start_rule_components text tl
    else start_rule_components text tl
  | [] -> []
;;

let string_list_contains symbol list =
  let open Base in
  List.mem list symbol ~equal:(fun x y -> String.equal x y)
;;

(* Берет все правила, которые имеют имя rule_name *)
let get_all_nonterms rule_name text =
  let _, rules = take_grammar text in
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

let rec try_apply_rule text rule input =
  let lhs, rhs = rule in
  match rhs with
  | h :: tl ->
    if list_empty input (* OVERSHOOT RIGHT HERE. *)
    then false, -1
    else if string_list_contains h (terminals text) (* TERM SYMBOL *)
    then
      if String.equal (List.hd input) h
      then
        try_apply_rule text (lhs, tl) (List.tl input)
        (* If equal TERM symbols in text and rule then continue checking *)
      else false, 0 (* If not equal then false, 0 --- REJECT RIGHT HERE. *)
    else if string_list_contains h (nonterminals text) (* NONTERM SYMBOL *)
    then (
      (* Get new input if nonterm rule is fits right here. *)
      let rec get_new_input all_nonterms ret =
        match all_nonterms with
        | h' :: tl' ->
          let is_applicable, remaining_input_len = try_apply_rule text h' input in
          if is_applicable
          then get_last_elements_from_list remaining_input_len input, ret
          else (
            let ret' = if ret = -1 then ret else remaining_input_len in
            get_new_input tl' ret')
        | [] -> input, ret
      in
      let new_input, ret = get_new_input (get_all_nonterms h text) 0 in
      if List.compare_lengths new_input input = 0
      then false, ret
      else try_apply_rule text (lhs, tl) new_input)
    else false, 0 (* REJECT *)
  | [] -> true, List.length input (* remaining input len *)
;;

exception NeverHappenError

let rec apply_rule text rule input =
  let lhs, rhs = rule in
  match rhs with
  | h :: tl ->
    if not (string_list_contains h (nonterminals text))
    then Term h :: apply_rule text (lhs, tl) (List.tl input)
    else (
      let rec apply = function
        | (lh', rh') :: tl' ->
          let is_applicable, remaining_input_len = try_apply_rule text (lh', rh') input in
          if is_applicable
          then
            Nonterm (h, apply_rule text (lh', rh') input)
            :: apply_rule
                 text
                 (lhs, tl)
                 (get_last_elements_from_list remaining_input_len input)
          else apply tl'
        | _ -> raise NeverHappenError
        (* Never happen because we checked it earlier in try_apply_rule function. *)
      in
      apply (get_all_nonterms h text))
  | [] -> []
;;

let parse text (g : grammar) (input : string list) =
  let _, all_rules = g in
  let rec apply input = function
    | h :: tl ->
      if let is_applicable, applied_rule_len = try_apply_rule text h input in
         is_applicable && applied_rule_len = 0
      then apply_rule text h input
      else apply input tl
    | [] -> raise NeverHappenError
    (* Never happen because we checked it earlier. *)
  in
  apply input (start_rule_components text all_rules)
;;

let parse_tree text (g : grammar) (input : string list) =
  let tree_list = parse text g input in
  let main_tree = Nonterm (start_rule text, tree_list) in
  let rec print_tree = function
    | Term s -> String.concat s [ " "; " " ]
    | Nonterm (s, parse_tree_list) ->
      String.concat
        ""
        [ " [ "
        ; s
        ; " : "
        ; String.concat " " (List.map (fun x -> print_tree x) parse_tree_list)
        ; " ] "
        ]
  in
  print_tree main_tree
;;

let try_apply_start_nonterm text input =
  let rec applier start_nonterms return_code =
    match start_nonterms with
    | h :: tl ->
      let cond, ret = try_apply_rule text h input in
      if cond
      then if ret = 0 then true, ret else applier tl ret
      else if return_code = -1
      then applier tl return_code
      else applier tl ret
    | [] -> false, return_code
  in
  applier (get_all_nonterms (start_rule text) text) 0
;;

let gen_parser text = try_apply_start_nonterm text
let gen_tree_parser text = parse_tree text (take_grammar text)

open Lexer
open Parser

let get_parser_and_tree_parser text =
  try Ok (gen_parser text, gen_tree_parser text) with
  | InvalidToken (l, s) ->
    Error
      (String.concat
         ""
         [ "Lexer Error: line "
         ; l
         ; " at: "
         ; s
         ; ". You can use command 'dune exec ./REPL.exe help' for get more information \
            about required syntax."
         ])
  (* Error from lexer. *)
  | Error ->
    (* Error from parser. *)
    Error
      "Parse Error: make sure you write nonterms with lowercase letters only and terms \
       with uppercase only (don't use any other symbols). You can use command 'dune exec \
       ./REPL.exe help' for get more information about required syntax."
    (* Only in this situation we have parse error, in other case there is InvalidToken exception. *)
  | NoSeparator s -> Error s
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
  let parser = gen_parser test_text in
  let res, _ = parser [ "PLUS"; "INT"; "INT"; "EOL" ] in
  res
;;

let%test _ =
  let tree_parser = gen_tree_parser test_text in
  tree_parser [ "PLUS"; "INT"; "INT"; "EOL" ]
  = " [ main :  [ expr :  PLUS   [ expr :  INT  ]   [ expr :  INT  ]  ]   EOL  ] "
;;

let%test _ =
  let parser = gen_parser test_text in
  let res, _ = parser [ "EOL" ] in
  res
;;

let%test _ =
  let tree_parser = gen_tree_parser test_text in
  tree_parser [ "EOL" ] = " [ main :  EOL  ] "
;;

let%test _ =
  let parser = gen_parser test_text in
  let res, _ = parser [ "PLUS"; "INT"; "INT" ] in
  not res (* OVERSHOOT *)
;;

let%test _ =
  let parser = gen_parser test_text in
  let res, _ = parser [ "HELLOWORLD" ] in
  not res (* REJECT *)
;;

let%test _ =
  let parser = gen_parser test_text in
  let res, _ = parser [ "LBRACE"; "PLUS"; "INT"; "MUL"; "INT"; "INT"; "RBRACE"; "EOL" ] in
  res
;;

let%test _ =
  let tree_parser = gen_tree_parser test_text in
  tree_parser [ "LBRACE"; "PLUS"; "INT"; "MUL"; "INT"; "INT"; "RBRACE"; "EOL" ]
  = " [ main :  [ expr :  LBRACE   [ expr :  PLUS   [ expr :  INT  ]   [ expr :  MUL   [ \
     expr :  INT  ]   [ expr :  INT  ]  ]  ]   RBRACE  ]   EOL  ] "
;;

let%test _ =
  let parser = gen_parser test_text in
  let res, return_code =
    parser [ "LBRACE"; "PLUS"; "INT"; "MUL"; "INT"; "INT"; "RBRACE"; "EOL"; "EOL" ]
  in
  (not res) && return_code = 0 (* REJECT *)
;;

let test_text = "%token PLU#!@#!KLS"

let%test _ =
  try
    let _, _ = gen_parser test_text [ "PLUS" ] in
    false
  with
  | NoSeparator _ -> true (* Not found %% *)
  | _ -> false
;;

let test_text = "%token PLU#!@#!KLS %%"

let%test _ =
  try
    let _, _ = gen_parser test_text [ "PLUS" ] in
    false
  with
  | InvalidToken (_, s) -> String.equal s "#!@#!KLS" (* Lexer error *)
;;

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
  let parser = gen_parser test_text in
  let res, _ = parser [ "WINNIE"; "TIGER"; "RABBIT"; "DONKEY"; "EOL" ] in
  res (* ACCEPT *)
;;

let%test _ =
  let parser = gen_parser test_text in
  let res, ret = parser [ "WINNIE"; "TIGER"; "RABBIT"; "DONKEY" ] in
  (not res) && ret = -1 (* OVERSHOOT *)
;;

let%test _ =
  let parser = gen_parser test_text in
  let res, ret = parser [ "WINNIE"; "TIGER"; "RABBIT"; "DONKEY"; "EOL"; "EOL" ] in
  (not res) && ret = 0 (* REJECT *)
;;

let%test _ =
  let parser = gen_parser test_text in
  let res, ret = parser [ "WINNIE"; "TIGER"; "RABBIT"; "DONKEY"; "QWERTY" ] in
  (not res) && ret = 0 (* REJECT *)
;;

let%test _ =
  let parser = gen_parser test_text in
  let res, ret = parser [ "WINNIE"; "TIGER"; "RABBIT"; "LITTLE_MY"; "EOL" ] in
  (not res) && ret = 0 (* REJECT *)
;;

let%test _ =
  let parser = gen_parser test_text in
  let res, ret = parser [ "MOOMINTROLL"; "TIGER"; "MOOMINPAPPA"; "LITTLE_MY"; "EOL" ] in
  (not res) && ret = 0 (* REJECT *)
;;

let%test _ =
  let parser = gen_parser test_text in
  let res, _ = parser [ "MOOMINTROLL"; "SNORK"; "MOOMINPAPPA"; "LITTLE_MY"; "EOL" ] in
  res (* ACCEPT *)
;;