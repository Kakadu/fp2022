open Core
open Str
open Angstrom
open Ast

let parse p s = parse_string ~consume:All p s

exception GetPathError of string

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

(* Считываем путь к файлу. *)
let () =
  Printf.printf
    "Enter the full directory to the .mly file from which you want to get the parser:\n"
;;

let rec read_mly_file path = In_channel.read_all path

(* let get_input_splitted_by_whitespaces s = Str.split regexp_whitespaces s *)

(* Строка с текстом. *)
let file_text =
  match In_channel.input_line In_channel.stdin with
  | Some path -> read_mly_file path
  | None -> raise (GetPathError "No input\n")
;;

(* Start position *)
let start_position_of_mly_tokens = 0

(* Position where %% *)
let end_position_of_mly_tokens =
  Str.search_forward regexp_procentprocent file_text start_position_of_mly_tokens
;;

(* the text where %token and %start only available. *)
let file_text_where_only_tokens_names =
  ref
    (String.sub
       file_text
       ~pos:start_position_of_mly_tokens
       ~len:end_position_of_mly_tokens)
;;

(* the text where only rules available. *)
let file_text_where_only_rules =
  ref
    (String.sub
       file_text
       ~pos:(end_position_of_mly_tokens + 2)
       ~len:(String.length file_text - end_position_of_mly_tokens - 2))
;;

(* Если строка не содержит строку, которую можно найти с помощью переданного регулярного
    выражения, то возвращаем -1. *)
let is_string_contains regexp str =
  try Str.search_forward regexp str 0 with
  | Not_found -> -1
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
let rec get_start_rule str =
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
let token_list =
  List.map
    ~f:(fun s -> Str.replace_first (Str.regexp "%token[\n\t\r ]+") "" s)
    (get_tokens_list file_text_where_only_tokens_names)
;;

(* Берем название стартового правила. *)
let start_rule =
  Str.replace_first
    (Str.regexp "%start[\n\t\r ]+")
    ""
    (get_start_rule file_text_where_only_tokens_names)
;;

(* Проверка на ошибки. *)
let do_check_for_parse_errors_tokens =
  check_string_with_tokens_on_errors file_text_where_only_tokens_names
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

(* Используем предыдущую функцию. *)
let mly_grammar = start_rule, get_rules_list file_text_where_only_rules
(*
type mlytree =
  | Node_rule of string * mlytree list
  | Leaf_token of string

let input_to_interpret =
  let str =
    match In_channel.input_line In_channel.stdin with
    | Some path -> read_mly_file path
    | None -> raise (GetPathError "No input\n")
  in
  String.split ~on:' ' str
;;

let get_next_word = "Nice"

let rec is_string_list_contains_element ~el = function
  | h :: tl -> if String.equal el h then true else is_string_list_contains_element ~el tl
  | [] -> false
;;

let is_element_a_token ~el = is_string_list_contains_element ~el token_list

let is_element_a_rule ~el =
  let rule_name_list =
    List.map
      ~f:(fun l ->
        let rule_name, _ = l in
        rule_name)
      rules
  in
  is_string_list_contains_element ~el rule_name_list
;;

exception NoRule of string

let rec get_rule rule_name =
  match List.find rules ~f:(fun (r, _) -> if r == rule_name then true else false) with
  | Some x -> x
  | None -> raise (NoRule "NORULE")
;;
*)

(*
let rec check_on_rule str_list rule = let (_, rule_part) = rule in match rule_part with
  | h :: tl -> if is_element_a_rule h then check_on_rule str_list (get_rule h)
  
*)
(*
let rec check_on_rule_component str rule_name = let (_, rule) = get_rule rule_name in 
  List.map ~f:(fun l -> List.for_all 
    ~f:(fun s -> if (is_element_a_token ~el:s && String.equal s str) then true else
      (is_element_a_token ~el:s && not (String.equal s str) then false else 
      (is_element_a_rule ~el:s && String.equal s rule_name then)
        )
      )
      )
  )
*)

(*
let rec check_on_rule rule_name rule_components is_first_check words =
  let (rule_comp, next_rule_comps) = (List.hd_exn rule_components) in
  let (word, next_words) = (List.hd_exn words, List.tl_exn words)  in
  match rule_comp with
  | h :: tl -> if is_element_a_token ~el:h && String.equal h word then (check_on_rule rule_name tl false next_words) else 
               if is_element_a_token ~el:h && not (String.equal h word) then false else 
               if is_element_a_rule ~el:h && (String.equal rule_name h) && is_first_check then false else
               if is_element_a_rule ~el:h && (String.equal rule_name h) && not is_first_check then (* check on this rule *) else
               (* check on other rule *)
  | [] -> true


*)

(*/Users/noname/Downloads/text.mly*)

open Stdlib
open Ast

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

(*TODELETE*)
let g =
  ( "main"
  , [ "main", [ "expr"; "EOL" ]
    ; "main", [ "EOL" ]
    ; "expr", [ "LBRACE"; "expr"; "RBRACE" ]
    ; "expr", [ "PLUS"; "PLUS"; "expr"; "expr" ]
    ; "expr", [ "PLUS"; "PLUS"; "MUL"; "expr"; "expr" ]
    ; "expr", [ "INT" ]
    ] )
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

let nonterminals = getNonterminals g

let rec is_symbol_nonterm symbol (g : grammar) nonterminals =
  match nonterminals with
  | h :: tl -> if String.equal h symbol then true else is_symbol_nonterm symbol g tl
  | [] -> false
;;

(* TO DO *)
(* Берет все правила, которые имеют имя rule_name *)
let rec getAllNonterminalsOfRule rule_name =
  let _, rules = g in
  List.filter
    (fun rule ->
      let nonterm, _ = rule in
      if String.equal nonterm rule_name then true else false)
    rules
;;

(* Возвращает *)
(* Здесь мы должны быть уверены, что длина списка input >= длине списка rhs. *)
let rec tryApplyRule rule input =
  let lhs, rhs = rule in
  match rhs with
  | h :: tl ->
    if not (is_symbol_nonterm h g nonterminals)
    then
      if String.equal (List.hd input) h
      then tryApplyRule (lhs, tl) (List.tl input)
      else false
    else (
      let rec getNewInputIfNonterminalRuleIsFits allNonterminalsOfRule =
        match allNonterminalsOfRule with
        | h' :: tl' ->
          if tryApplyRule h' input
          then
            get_last_n_elements_from_list
              (List.length input - List.length (getRhs h'))
              input
          else getNewInputIfNonterminalRuleIsFits tl'
        | [] -> input
      in
      if List.length (getNewInputIfNonterminalRuleIsFits (getAllNonterminalsOfRule h))
         = List.length input
      then false
      else
        tryApplyRule
          (lhs, tl)
          (getNewInputIfNonterminalRuleIsFits (getAllNonterminalsOfRule h)))
  | [] -> true
;;

let rec applyRule rule input =
  let lhs, rhs = rule in
  match rhs with
  | h :: tl ->
    if not (is_symbol_nonterm h g nonterminals)
    then Term h :: applyRule (lhs, tl) (List.tl input)
    else (
      let rec x newRules =
        match newRules with
        | (lh', rh') :: tl' ->
          if tryApplyRule (lh', rh') input
          then
            Nonterm (h, applyRule (lhs, rh') input) :: applyRule (lhs, tl) (List.tl input)
          else x tl'
        | _ ->
          failwith
            "Should never happen because we checked it earlier in tryApplyRule function."
      in
      x (getAllNonterminalsOfRule h))
  | [] -> []
;;

let rec parse (g : grammar) (input : string list) =
  let _, allRules = g in
  let rec rulesApplier input rules =
    match rules with
    | h :: tl -> if tryApplyRule h input then applyRule h input else rulesApplier input tl
    | [] -> failwith "No such rule for your input"
  in
  rulesApplier input allRules
;;

let rec parseTree (input : string list) =
  let tree_list = parse g input in
  let main_tree = Nonterm (start_rule, tree_list) in
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

(*TODELETE*)
(* Prints map which has type symbolMap. *)
(* This function is only used for check on correctness. *)
let symbol_map_print (x : SymbolSet.t SMap.t) =
  SMap.iter
    (fun key data ->
      print_string ("key: " ^ key ^ " ");
      SymbolSet.iter (fun s -> print_string ("value: " ^ s ^ " ")) data;
      print_string "\n")
    x
;;

(* For parsing we are using LL(1)-grammar, so we should build a FIRST and FOLLOW sets. *)
(* We are going to use type symbolMap, where keys would be our rules and values would be sets itself. *)

(*--------------------------------------------FIRST------------------------------------------------------*)

(* Initialize a map and add keys: keys are rules and values are string sets *)
(* 
  For our example there is something like this: 
    key: expr
    key: main
  (without values on initialization step) 
*)
let getInitFirstSets (g : grammar) =
  List.fold_right
    (fun s map -> SMap.add s SymbolSet.empty map)
    (getNonterminals g)
    SMap.empty
;;

(*
  This function build FIRST set: it takes a (possibly incomplete) map of FIRST sets (first), and a sequence of symbols (symbolSeq),
  and returns the FIRST set of symbolSeq, according to the given FIRST sets.
  Algoritm to find FIRST SET:
    let A = A1...An, where Ai may be term or nonterm symbols.
    So, if A = eps then FIRST(A) = { eps }.
    Otherwise:
      if A1 is a term then FIRST(A) = { A1 }. 
      else we need to know is eps in FIRST(A1):
        if eps in FIRST(A1) then FIRST(A) = (FIRST(A1) without { eps }) unite FIRST(A2 · · · An) 
        else FIRST(A) = FIRST(A1).
*)
let rec computeFirstSet (first : symbolMap) (symbolSeq : string list) : SymbolSet.t =
  (* symbolSeq here is A1...An. *)
  match symbolSeq with
  (* singleton x returns the one-element set containing only x. *)
  | [] -> SymbolSet.singleton "eps"
  | h :: t ->
    (match SMap.find h first with
     | exception Not_found -> SymbolSet.singleton h (* A1 is term. *)
     | first_h ->
       (* mem x m returns true if m contains a binding for x, and false otherwise. *)
       if SymbolSet.mem "eps" first_h
       then SymbolSet.union (SymbolSet.remove "eps" first_h) (computeFirstSet first t)
       else first_h)
;;

let recurseFirstSets (g : grammar) (first : symbolMap) : symbolMap =
  let _, rules = g in
  let updateMap rule map =
    let rule_name, rule_component = rule in
    let newSet =
      SymbolSet.union (SMap.find rule_name map) (computeFirstSet first rule_component)
    in
    SMap.add rule_name newSet map
  in
  List.fold_right updateMap rules first
;;

let rec getFirstSets (g : grammar) (first : symbolMap) : symbolMap =
  let upd = recurseFirstSets g first in
  if SMap.equal SymbolSet.equal first upd then upd else getFirstSets g upd
;;

(*--------------------------------------------------------------------------------------------------------*)

(*----------------------------------------------FOLLOW----------------------------------------------------*)

(* 
  For our example there is something like this: 
    key: expr
    key: main value: eof
  (without values on initialization step) 
*)

let getInitFollowSets (g : grammar) : symbolMap =
  SMap.add (getStartSymbol g) (SymbolSet.singleton "eof") (getInitFirstSets g)
;;

(* Here nt is rule_name and symbolSeq is rule_component. *)
let rec updateFollowSet
  (first : symbolMap)
  (follow : symbolMap)
  (nt : string)
  (symbolSeq : string list)
  : symbolMap
  =
  match symbolSeq with
  | [] -> follow (* this will NOT happen *)
  | h :: t ->
    if SMap.mem h first
    then
      if (* Bi is NT *)
         not ([] = t)
      then (
        (* Bi is NOT last symbol of the rule *)
        let first_t = computeFirstSet first t in
        if SymbolSet.mem "eps" first_t
        then (
          (* ε ∈ FIRST(Bi+1...Bk) *)
          let updated =
            SMap.add
              h
              (SymbolSet.union
                 (SymbolSet.union (SMap.find h follow) (SymbolSet.remove "eps" first_t))
                 (SMap.find nt follow))
              follow
          in
          updateFollowSet first updated nt t)
        else (
          (* ε not ∈ FIRST(Bi+1...Bk) *)
          let updated =
            SMap.add h (SymbolSet.union (SMap.find h follow) first_t) follow
          in
          updateFollowSet first updated nt t))
      else
        (* Bi is last symbol of the rule *)
        SMap.add h (SymbolSet.union (SMap.find h follow) (SMap.find nt follow)) follow
    else (* Bi is T, skip it *)
      updateFollowSet first follow nt t
;;

let recurseFollowSets (g : grammar) (first : symbolMap) (follow : symbolMap) : symbolMap =
  let _, rules = g in
  let updateMap map rule =
    match rule with
    | lhs, rhs -> updateFollowSet first map lhs rhs
  in
  List.fold_left updateMap follow rules
;;

let rec getFollowSets (g : grammar) (first : symbolMap) (follow : symbolMap) : symbolMap =
  let compareSMap x y = SMap.equal SymbolSet.equal x y in
  let updated = recurseFollowSets g first follow in
  if compareSMap follow updated then updated else getFollowSets g first updated
;;

(*--------------------------------------------------------------------------------------------------------*)

(*-----------------------------------------------PREDICT--------------------------------------------------*)

let rec getPredictSets (g : grammar) (first : symbolMap) (follow : symbolMap)
  : ((string * string list) * SymbolSet.t) list
  =
  (* For each rule A ::= δ
    If ε ∈ FIRST(δ):
      PREDICT(A ::= δ) = (FIRST(δ) - {ε}) ∪ FOLLOW(A) 
    otherwise:
      PREDICT(A ::= δ) = FIRST(δ)  
  *)
  let _, rules = g in
  let addList rule l =
    let lhs, rhs = rule in
    let rhs_first = computeFirstSet first rhs in
    if SymbolSet.mem "eps" rhs_first
    then
      (rule, SymbolSet.union (SymbolSet.remove "eps" rhs_first) (SMap.find lhs follow))
      :: l
    else (rule, computeFirstSet first rhs) :: l
  in
  List.fold_right addList rules []
;;

module StringPairs = struct
  type t = string * string

  let compare (x0, y0) (x1, y1) =
    match String.compare x0 x1 with
    | 0 -> String.compare y0 y1
    | c -> c
  ;;
end

module PairsMap = Map.Make (StringPairs)

type predictMapType = string list PairsMap.t

(*--------------------------------------------------------------------------------------------------------*)

let rec derive
  (first : symbolMap)
  (predict : predictMapType)
  (pattern : string list)
  (input : string list)
  : bool
  =
  match pattern with
  | [] ->
    (match input with
     | [] -> true
     | x :: y -> false)
  | ph :: pt ->
    if SMap.mem ph first
    then (
      (* ph is NT *)
      match input with
      | [] -> false
      | ih :: it ->
        let expanded =
          try PairsMap.find (ph, ih) predict with
          | Not_found -> [ ph ]
        in
        (match expanded with
         | [] -> derive first predict pt input (* check rest of pattern *)
         | eh :: et ->
           if String.equal ph eh
           then false (* found NO rewrite *)
           else derive first predict (expanded @ pt) input (* check rewritten pattern *)))
    else (
      (* ph is T *)
      match input with
      | [] -> false
      | ih :: it ->
        if String.equal ph ih
        then derive first predict pt it (* check rest of both pattern and input *)
        else false)
;;

let tryDerive (g : grammar) (inputStr : string list) : bool =
  let ifs = getInitFirstSets g in
  let fs = getFirstSets g ifs in
  let ifl = getInitFollowSets g in
  let ls = getFollowSets g fs ifl in
  let pset = getPredictSets g fs ls in
  let addPredictMap pmap rule =
    let (lhs, rhs), predictList = rule in
    let addPairsMap predictSymbol map = PairsMap.add (lhs, predictSymbol) rhs map in
    SymbolSet.fold addPairsMap predictList pmap
  in
  let predictMap = List.fold_left addPredictMap PairsMap.empty pset in
  derive fs predictMap [ getStartSymbol g; "eof" ] (inputStr @ [ "eof" ])
;;

exception SyntaxError of string

let rec deriveTree
  (first : symbolMap)
  (predict : predictMapType)
  (symbol : string)
  (input : string list)
  : parseTree * string list
  =
  if SMap.mem symbol first
  then (
    (* NT *)
    match input with
    | [] -> raise (SyntaxError "No more input to match")
    | ih :: it ->
      let expanded =
        try PairsMap.find (symbol, ih) predict with
        | Not_found -> [ symbol ]
      in
      (match expanded with
       | [] -> Nonterm (symbol, []), input
       | eh :: et ->
         if String.equal symbol eh
         then
           raise (SyntaxError (symbol ^ " has no rule to rewrite"))
           (* found NO rewrite *)
         else (
           let createTree (subTree, input) symbol =
             let childTree, newInput = deriveTree first predict symbol input in
             subTree @ [ childTree ], newInput
           in
           let expandedTree, newInput = List.fold_left createTree ([], input) expanded in
           Nonterm (symbol, expandedTree), newInput)))
  else (
    (* T *)
    match input with
    | [] -> raise (SyntaxError (symbol ^ " has no match")) (* found no match *)
    | ih :: it -> Term symbol, it)
;;

let tryDeriveTree (g : grammar) (inputStr : string list) : parseTree =
  let ifs = getInitFirstSets g in
  let fs = getFirstSets g ifs in
  let ifl = getInitFollowSets g in
  let ls = getFollowSets g fs ifl in
  let pset = getPredictSets g fs ls in
  let addPredictMap pmap rule =
    let (lhs, rhs), predictList = rule in
    let addPairsMap predictSymbol map = PairsMap.add (lhs, predictSymbol) rhs map in
    SymbolSet.fold addPairsMap predictList pmap
  in
  let predictMap = List.fold_left addPredictMap PairsMap.empty pset in
  let tree, input = deriveTree fs predictMap (getStartSymbol g) (inputStr @ [ "eof" ]) in
  tree
;;

let genParser g = tryDerive g
let genTreeParser g = tryDeriveTree g

(* util code *)
let printPredictSet predict =
  List.iter
    (fun x ->
      let (nt, rhs), pset = x in
      print_endline ("Rule: " ^ nt ^ " -> " ^ String.concat " " rhs);
      SymbolSet.iter (fun str -> print_endline ("  Predict: " ^ str)) pset)
    predict
;;

(* testing code *)

let rec printParseTree tree =
  match tree with
  | Term s -> " " ^ s ^ " "
  | Nonterm (s, node) ->
    let str = String.concat "" (List.map (fun x -> printParseTree x) node) in
    " [ " ^ s ^ " : " ^ str ^ " ] "
;;

(*
open Lexer

let g = mly_grammar

open Core

let input_ =
  match In_channel.input_line In_channel.stdin with
  | Some input -> String.split ~on:' ' input
  | None -> raise (GetPathError "No input\n")
;;

let t = tryDerive g input_
let r = tryDeriveTree g input_
let y = printParseTree r

*)

(*TODELETE*)

(* Initialize a map and add keys: keys are rules and values are string sets *)
(* 
  In our example, after : 
  key: 
    expr : string
  value: 
    LBRACE; expr; RBRACE : string set
  value:
    MUL; expr; expr : string set
  value:
    PLUS; expr; expr : string set
      INT : string set

  key:
    main : string
  value:
    expr; EOL : string set
  value:
    EOL : string set
  
  Note that set is a functor building an implementation of the set structure given a TOTALLY ORDERED TYPE, so everything is okay there.
*)