open Core
open Str

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