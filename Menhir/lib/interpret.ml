open Parser

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
  let command_args = split_string_and_delete_spaces command in
  match command_args with
  | h :: tl when h = "menhir" ->
    if List.length tl = 0
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
                ("text", try_read_file_text (Unix.openfile s [] 0)) :: interpret_flags tl'
              with
              | _ -> raise (UnknownCommand s)))
        | _ -> []
      in
      interpret_flags tl)
  | [ s ] -> raise (UnknownCommand s)
  | _ -> raise (UnknownCommand "")
;;

exception NoFile of string

open Ast
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
let get_start_symbol (g : grammar) =
  let start, _ = g in
  start
;;

(* Takes all nonterminals symbols. *)
(* From example we take ["main"; "main"; "expr"; "expr"; "expr"; "expr"] *)

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

let nonterminals text = get_nonterminals (take_grammar text)
let terminals text = token_list text

let rec start_rule_components text = function
  | (lhs, rhs) :: tl ->
    if String.equal (start_rule text) lhs
    then (lhs, rhs) :: start_rule_components text tl
    else start_rule_components text tl
  | [] -> []
;;

let rec is_string_list_contains_symbol symbol l =
  match l with
  | h :: tl ->
    if String.equal h symbol then true else is_string_list_contains_symbol symbol tl
  | [] -> false
;;

(* Берет все правила, которые имеют имя rule_name *)
let get_all_nonterminals_of_rule rule_name text =
  let _, rules = take_grammar text in
  List.filter
    (fun rule ->
      let nonterm, _ = rule in
      if String.equal nonterm rule_name then true else false)
    rules
;;

let rec try_apply_rule text rule input =
  let lhs, rhs = rule in
  match rhs with
  | h :: tl ->
    if List.length input = 0 (* OVERSHOOT RIGHT HERE. *)
    then false, -1
    else if is_string_list_contains_symbol h (terminals text)
    then
      if String.equal (List.hd input) h
      then try_apply_rule text (lhs, tl) (List.tl input)
      else false, 0
    else if is_string_list_contains_symbol h (nonterminals text)
    then (
      let rec getNewInputIfNonterminalRuleIsFits allNonterminalsOfRule return_code =
        match allNonterminalsOfRule with
        | h' :: tl' ->
          let flag, remaining_input_len = try_apply_rule text h' input in
          if flag
          then get_last_n_elements_from_list remaining_input_len input, return_code
          else if return_code = -1
          then getNewInputIfNonterminalRuleIsFits tl' return_code
          else getNewInputIfNonterminalRuleIsFits tl' remaining_input_len
        | [] -> input, return_code
      in
      let new_input, return_code =
        getNewInputIfNonterminalRuleIsFits (get_all_nonterminals_of_rule h text) 0
      in
      if List.length new_input = List.length input
      then false, return_code
      else try_apply_rule text (lhs, tl) new_input)
    else false, 0
  | [] -> true, List.length input (* remaining input len *)
;;

let rec apply_rule text rule input =
  let lhs, rhs = rule in
  match rhs with
  | h :: tl ->
    if not (is_string_list_contains_symbol h (nonterminals text))
    then Term h :: apply_rule text (lhs, tl) (List.tl input)
    else (
      let rec x newRules =
        match newRules with
        | (lh', rh') :: tl' ->
          let flag, remaining_input_len = try_apply_rule text (lh', rh') input in
          if flag
          then
            Nonterm (h, apply_rule text (lh', rh') input)
            :: apply_rule
                 text
                 (lhs, tl)
                 (get_last_n_elements_from_list remaining_input_len input)
          else x tl'
        | _ ->
          failwith
            "Should never happen because we checked it earlier in try_apply_rule \
             function."
      in
      x (get_all_nonterminals_of_rule h text))
  | [] -> []
;;

let parse text (g : grammar) (input : string list) =
  let _, allRules = g in
  let rec rulesApplier input rules =
    match rules with
    | h :: tl ->
      if let flag, applied_rule_len = try_apply_rule text h input in
         flag && applied_rule_len = 0
      then apply_rule text h input
      else rulesApplier input tl
    | [] -> failwith "No such rule for your input"
  in
  rulesApplier input (start_rule_components text allRules)
;;

let parse_tree text (g : grammar) (input : string list) =
  let tree_list = parse text g input in
  let main_tree = Nonterm (start_rule text, tree_list) in
  let rec printTree tree =
    match tree with
    | Term s -> " " ^ s ^ " "
    | Nonterm (s, parse_treeList) ->
      " [ "
      ^ s
      ^ " : "
      ^ String.concat " " (List.map (fun x -> printTree x) parse_treeList)
      ^ " ] "
  in
  printTree main_tree
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
  applier (get_all_nonterminals_of_rule (start_rule text) text) 0
;;

let gen_parser text = try_apply_start_nonterm text
let gen_tree_parser text = parse_tree text (take_grammar text)

let get_parser_and_tree_parser command =
  try
    let _, text =
      List.find
        (fun x ->
          let l, _ = x in
          if l = "text" then true else false)
        (command_list command)
    in
    try Ok (gen_parser text, gen_tree_parser text) with
    | ParseError s -> Error s
    | NoStartToken s -> Error s
  with
  | Not_found -> Error "ATTENTION: No path in your command"
  | UnknownCommand s -> Error ("Unknown command, switch or bad path: " ^ s)
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
  \    | expr; EOL\n\
  \    | EOL\n\
   expr:\n\
  \    | LBRACE; expr; RBRACE\n\
  \    | PLUS; expr; expr\n\
  \    | MUL; expr; expr\n\
  \    | INT"
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
  res = false (* OVERSHOOT *)
;;

let%test _ =
  let parser = gen_parser test_text in
  let res, _ = parser [ "HELLOWORLD" ] in
  res = false (* REJECT *)
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
