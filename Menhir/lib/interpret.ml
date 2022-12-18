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
      (NoSeparator
         "Error: There is no separator in text (make sure you don't forget \"%%\")")
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

exception UnknownCommand of string

let read_all_file_text file_path =
  Stdio.In_channel.input_all (Unix.in_channel_of_descr file_path)
;;

let try_read_file_text file_path =
  try read_all_file_text file_path with
  | Unix.Unix_error (Unix.EBADF, "read", "") -> ""
;;

let read_command () =
  try read_line () with
  | End_of_file -> ""
;;

let split_string_and_delete_spaces command =
  List.filter
    (fun x -> not (String.equal x " " || String.equal x ""))
    (String.split_on_char ' ' command)
;;

let command_list command =
  if String.equal command "exit"
  then exit 0
  else (
    let command_args = split_string_and_delete_spaces command in
    match command_args with
    | h :: tl when h = "menhir" ->
      if tl = []
      then (
        let () = print_endline "ATTENTION: No flags in your command" in
        [])
      else (
        let rec interpret_flags flags =
          match flags with
          | h' :: tl' ->
            (match h' with
             | "--interpret" -> ("switch", "--interpret") :: interpret_flags tl'
             | s ->
               (try
                  ("text", try_read_file_text (Unix.openfile s [] 0))
                  :: interpret_flags tl'
                with
                | _ -> raise (UnknownCommand s)))
          | _ -> []
        in
        interpret_flags tl)
    | h :: _ -> raise (UnknownCommand h)
    | _ -> raise (UnknownCommand ""))
;;

exception NoFile of string

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

let rec string_list_contains symbol l =
  match l with
  | h :: _ when String.equal h symbol -> true
  | _ :: tl -> string_list_contains symbol tl
  | [] -> false
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

let rec apply_rule text rule input =
  let lhs, rhs = rule in
  match rhs with
  | h :: tl ->
    if not (string_list_contains h (nonterminals text))
    then Term h :: apply_rule text (lhs, tl) (List.tl input)
    else (
      let rec x newRules =
        match newRules with
        | (lh', rh') :: tl' ->
          let is_applicable, remaining_input_len = try_apply_rule text (lh', rh') input in
          if is_applicable
          then
            Nonterm (h, apply_rule text (lh', rh') input)
            :: apply_rule
                 text
                 (lhs, tl)
                 (get_last_elements_from_list remaining_input_len input)
          else x tl'
        | _ ->
          failwith
            "Should never happen because we checked it earlier in try_apply_rule \
             function."
      in
      x (get_all_nonterms h text))
  | [] -> []
;;

let parse text (g : grammar) (input : string list) =
  let _, allRules = g in
  let rec rulesApplier input rules =
    match rules with
    | h :: tl ->
      if let is_applicable, applied_rule_len = try_apply_rule text h input in
         is_applicable && applied_rule_len = 0
      then apply_rule text h input
      else rulesApplier input tl
    | [] -> failwith "No such rule for your input"
  in
  rulesApplier input (start_rule_components text allRules)
;;

let parse_tree text (g : grammar) (input : string list) =
  let tree_list = parse text g input in
  let main_tree = Nonterm (start_rule text, tree_list) in
  let rec print_tree tree =
    match tree with
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

let get_parser_and_tree_parser command =
  try
    let _, text =
      List.find
        (fun x ->
          let l, _ = x in
          String.equal l "text")
        (command_list command)
    in
    try Ok (gen_parser text, gen_tree_parser text) with
    | InvalidToken (l, s) -> Error ("Lexer Error: " ^ "line " ^ l ^ " at: " ^ s)
    (* Error from lexer. *)
    | Error ->
      (* Error from parser. *)
      Error
        "Parse Error: make sure you write nonterms with lowercase letters only and terms \
         with uppercase only (don't use any other symbols)"
    (* Only in this situation we have parse error, in other case there is InvalidToken exception. *)
  with
  | Not_found -> Error "ATTENTION: No path in your command"
  | UnknownCommand s -> Error ("Unknown command, switch or bad path: " ^ s)
  | NoSeparator s -> Error s
;;

(* TESTS *)
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
   | expr; EOL\n\
   | EOL\n\
   expr:\n\
   | LBRACE; expr; RBRACE\n\
   | PLUS; expr; expr\n\
   | MUL; expr; expr\n\
   | INT"
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
