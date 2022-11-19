open Angstrom
open Ast

let is_whitespace = function
  | ' ' -> true
  | _ -> false
;;

let is_tab = function
  | '\t' -> true
  | _ -> false
;;

let is_newline = function
  | '\n' | '\r' -> true
  | _ -> false
;;

let not_newline = Fun.negate is_newline
let is_empty_char c = is_whitespace c || is_tab c || is_newline c

let is_hash = function
  | '#' -> true
  | _ -> false
;;

let is_colon = function
  | ':' -> true
  | _ -> false
;;

let is_backslash = function
  | '\\' -> true
  | _ -> false
;;

let some_pred preds el = List.exists (fun fn -> fn el) preds
let all_pred preds el = List.for_all (fun fn -> fn el) preds
let discard p = p *> return ()
let wrap p q = q *> p <* q
let sep_and_trim s p = wrap (sep_by s p) s
let eols = many end_of_line
let comment = char '#' *> skip_while not_newline <* eols

let skip_meaningless_characters =
  let empty_characters = is_empty_char |> take_while1 |> discard in
  many (empty_characters <|> comment)
;;

let trim_start, trim_end =
  let start p = skip_meaningless_characters *> p in
  let finish p = p <* skip_meaningless_characters in
  start, finish
;;

let filename =
  take_while1
    (all_pred
       [ Fun.negate is_empty_char
       ; Fun.negate is_hash
       ; Fun.negate is_colon
       ; Fun.negate is_backslash
       ])
;;

let filename_delim =
  option () (some_pred [ is_whitespace; is_tab ] |> take_while1 |> discard)
;;

let filenames = sep_and_trim filename_delim filename
let targets = both filename filenames <* char ':'

let prerequisites =
  fix (fun p ->
    lift2
      List.append
      (filename_delim *> many comment *> filenames)
      ((char '\\' <* filename_delim *> char '\n') *> p <|> many comment *> return []))
;;

let recipes =
  let empty_line =
    let ws_line = char ' ' *> filename_delim in
    wrap ws_line eols <|> discard (many1 end_of_line)
  in
  let recipe_delim = many (empty_line <|> comment) in
  let recipe_line = char '\t' *> take_while not_newline in
  sep_and_trim recipe_delim recipe_line
;;

let rule =
  let rule_constructor t p r = { targets = t; prerequisites = p; recipes = r } in
  lift3 rule_constructor targets prerequisites recipes
;;

let expr =
  let ast_constructor t p r = Rule { targets = t; prerequisites = p; recipes = r } in
  lift3 ast_constructor targets prerequisites recipes
;;

let parser = trim_start (both rule (many expr))
