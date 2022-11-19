(** Copyright 2021-2022, ol-imorozko and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

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

let test_ok, test_fail =
  let ok ppf parser input expected =
    match parse_string ~consume:All parser input with
    | Ok res when expected = res -> true
    | Ok res ->
      ppf Format.std_formatter res;
      false
    | Error e ->
      print_string e;
      false
  in
  let fail ppf parser input =
    match parse_string ~consume:All parser input with
    | Ok res ->
      ppf Format.std_formatter res;
      false
    | _ -> true
  in
  ok, fail
;;

(* runs p and discards it's output *)
let discard p = p *> return ()

(* runs q around p, discards its result, then runs p, and returns its result *)
let wrap p q = q *> p <* q

(* runs p zero or more times, interspersing runs of s in between, trimming the output with s *)
let sep_and_trim s p = wrap (sep_by s p) s

(* skip some/none end of lines *)
let eols = many end_of_line

(* skip line starting with # *)
let comment = char '#' *> skip_while not_newline <* eols

(* skip empty characters or comment lines *)
let skip_meaningless_characters =
  let empty_characters = is_empty_char |> take_while1 |> discard in
  many (empty_characters <|> comment)
;;

(* trims empty characters or comment lines *)
let trim_start, trim_end =
  let start p = skip_meaningless_characters *> p in
  let finish p = p <* skip_meaningless_characters in
  start, finish
;;

let tmp = { targets = "abc", []; prerequisites = []; recipes = [] }
let rule = char 't' >>| fun _ -> Rule tmp
let parser = trim_start rule |> many1 |> trim_end
let parse_ok = test_ok (Format.pp_print_list pp_expr) parser
let parse_fail = test_fail (Format.pp_print_list pp_expr) parser

let%test _ = parse_ok "#\nt" [ Rule tmp ]
let%test _ = parse_ok "t#" [ Rule tmp ]
let%test _ = parse_ok "t#\n" [ Rule tmp ]
let%test _ = parse_ok "#\n #\n t #\n #\n t" [ Rule tmp; Rule tmp ]
let%test _ = parse_ok "#\n   #\n    #fdsf\n t" [ Rule tmp ]
let%test _ = parse_ok " #fdsfs\n    t    #fdfsf" [ Rule tmp ]
let%test _ = parse_ok " #fdsfs\n    t    #fdfsf  \n  " [ Rule tmp ]
let%test _ = parse_ok "  #39139\r   t   t  #" [ Rule tmp; Rule tmp ]
let%test _ = parse_fail "#"
let%test _ = parse_fail "    #fodsfjo \r#x  \n# f"
let%test _ = parse_fail "#fodsfjo\n \t \t \n "

(* ban '\' in filenames *)
let filename =
  take_while1
    (all_pred
       [ Fun.negate is_empty_char
       ; Fun.negate is_hash
       ; Fun.negate is_colon
       ; Fun.negate is_backslash
       ])
;;

(* filename delimeter *)
let filename_delim =
  option () (some_pred [ is_whitespace; is_tab ] |> take_while1 |> discard)
;;

(* parse filenames separated by delimeters *)
let filenames = sep_and_trim filename_delim filename

(* ========================================== *)
(* Parse targets `<target> [<target[s]>...]:` *)
(* ========================================== *)
let targets = both filename filenames <* char ':'

type targets = string * string list [@@deriving show { with_path = false }]

let parser = targets
let parse_ok = test_ok pp_targets parser
let parse_fail = test_fail pp_targets parser

let%test _ = parse_ok "abc:" ("abc", [])
let%test _ = parse_ok "abc  :" ("abc", [])
let%test _ = parse_ok "abc \t  :" ("abc", [])
let%test _ = parse_ok "a  \t:" ("a", [])
let%test _ = parse_ok "a\tb:" ("a", [ "b" ])
let%test _ = parse_ok "a \t b c,;dex :" ("a", [ "b"; "c,;dex" ])
let%test _ = parse_ok "abc \t f  f :" ("abc", [ "f"; "f" ])
let%test _ = parse_fail "abc\n:"
let%test _ = parse_fail "abc#:"
let%test _ = parse_fail ":#c:"
let%test _ = parse_fail ":"
let%test _ = parse_fail ":::"
let%test _ = parse_fail ""
let%test _ = parse_fail ":  \t fdsf \n"

(* ========================================= *)
(* Parse prerequisites (could be multilined) *)
(* We could not insert comments in between.  *)
(* ========================================= *)
let prerequisites =
  fix (fun p ->
    lift2
      List.append
      (filename_delim *> many comment *> filenames)
      ((char '\\' <* filename_delim *> char '\n') *> p <|> many comment *> return []))
;;

let parser = prerequisites
let parse_ok = test_ok (Format.pp_print_list (fun _ -> print_string)) parser
let parse_fail = test_fail (Format.pp_print_list (fun _ -> print_string)) parser

let%test _ = parse_ok "abc" [ "abc" ]
let%test _ = parse_ok "abc  " [ "abc" ]
let%test _ = parse_ok "    abc" [ "abc" ]
let%test _ = parse_ok "abc \t  " [ "abc" ]
let%test _ = parse_ok "a \t b c,;dex" [ "a"; "b"; "c,;dex" ]
let%test _ = parse_ok "abc \t f  f " [ "abc"; "f"; "f" ]
let%test _ = parse_ok "abc #\t f  f \t" [ "abc" ]
let%test _ = parse_ok "abc #\n" [ "abc" ]
let%test _ = parse_ok "#:f" []
let%test _ = parse_ok "" []
let%test _ = parse_ok "\t \t \\\n \t  " []
let%test _ = parse_fail "a\n"
let%test _ = parse_fail "\na"
let%test _ = parse_fail "\n\n\n a"
let%test _ = parse_fail "\n  \n a"
let%test _ = parse_fail "\n  \n \n\techo abc"
let%test _ = parse_fail ":"
let%test _ = parse_fail ":fsdf"
let%test _ = parse_fail "fdsf:::"
(* multiline tests *)
let%test _ = parse_ok "a\\\nb" [ "a"; "b" ]
let%test _ = parse_ok " \\\n\ta" [ "a" ]
let%test _ = parse_ok "  \\  \t \n  a" [ "a" ]
let%test _ = parse_ok "  \\\n  a" [ "a" ]
let%test _ = parse_ok "a\\\n  \t  \t " [ "a" ]
let%test _ = parse_ok "a\\\n  \t  \t b  " [ "a"; "b" ]
let%test _ = parse_ok "a  \\\n  b" [ "a"; "b" ]
let%test _ = parse_ok "a\\\n" [ "a" ]
let%test _ = parse_ok "a \\\n b c \\\n d e" [ "a"; "b"; "c"; "d"; "e" ]
let%test _ = parse_ok "a\\\n" [ "a" ]
let%test _ = parse_ok "a#\\\n" [ "a" ]
let%test _ = parse_fail "a\\#b\n"
let%test _ = parse_fail "a\\#b\n"
let%test _ = parse_fail "a\na"
let%test _ = parse_fail "a\\\n\na"
let%test _ = parse_fail "a\\\n\n  abc:"
let%test _ = parse_fail "a\\\n  b:"

type rule_wo_recepie =
  { targets : string * string list
  ; prerequisites : string list
  }
[@@deriving show { with_path = false }]

(* combine parsers to make <target> [<target[s]>...]: [<prerequisite[s]>...] parsing *)
let parser = lift2 (fun t p -> { targets = t; prerequisites = p }) targets prerequisites
let parse_ok = test_ok pp_rule_wo_recepie parser
let parse_fail = test_fail pp_rule_wo_recepie parser

let%test _ = parse_ok "a:b" { targets = "a", []; prerequisites = [ "b" ] }
let%test _ = parse_fail "a:b\n"

let%test _ =
  parse_ok "a \tb c: a" { targets = "a", [ "b"; "c" ]; prerequisites = [ "a" ] }
;;

let%test _ =
  parse_ok "a:a \\\n #kek\n c \t" { targets = "a", []; prerequisites = [ "a"; "c" ] }
;;

let%test _ =
  parse_ok "a:a \\\n b c" { targets = "a", []; prerequisites = [ "a"; "b"; "c" ] }
;;

let%test _ = parse_fail "abc\n:"
let%test _ = parse_fail "\ta b c d e\n"
let%test _ = parse_fail "abc#:"
let%test _ = parse_fail ":#c:"
let%test _ = parse_fail ":a b c#c:"
let%test _ = parse_fail ":a \\\n b\n"
let%test _ = parse_fail ":"
let%test _ = parse_fail ":::"
let%test _ = parse_fail ""
let%test _ = parse_fail ":  \t fdsf \n"

(* ============================================ *)
(* Parse recipes                                *)
(* After targets:prerequisites parsing, recipes *)
(* are lines __starting with the tab__.         *)
(* ============================================ *)
let recipes =
  let empty_line =
    let ws_line = char ' ' *> filename_delim in
    wrap ws_line eols <|> discard (many1 end_of_line)
  in
  let recipe_delim = many (empty_line <|> comment) in
  let recipe_line = char '\t' *> take_while not_newline in
  sep_and_trim recipe_delim recipe_line
;;

let parser = recipes
let parse_ok = test_ok (Format.pp_print_list (fun _ -> print_string)) parser
let parse_fail = test_fail (Format.pp_print_list (fun _ -> print_string)) parser

let%test _ = parse_ok "\tabc" [ "abc" ]
let%test _ = parse_ok "\tabc\n" [ "abc" ]
let%test _ = parse_ok "\t\t\tabc\n" [ "\t\tabc" ]
let%test _ = parse_ok "\tabc\n  #abcde\n" [ "abc" ]
let%test _ = parse_ok "\tabc\n#x\n" [ "abc" ]
let%test _ = parse_ok "\n   \n\t   abc\t\t#abc" [ "   abc\t\t#abc" ]
let%test _ = parse_ok "\n   \n\t#comment\n" [ "#comment" ]
let%test _ = parse_ok "#cmnt\n\n\tabc\n" [ "abc" ]
let%test _ = parse_ok "\ta\n\tb\n\tc\n  " [ "a"; "b"; "c" ]
let%test _ = parse_ok "\n\n\n    \n" []
let%test _ = parse_ok "\n   \t\n\ta" [ "a" ]
let%test _ = parse_fail "\n#abc\n  not a recipe cause not \t \n"

let rule_constructor t p r = { targets = t; prerequisites = p; recipes = r }
let parser = lift3 rule_constructor targets prerequisites recipes
let parse_ok = test_ok pp_rule parser
let parse_fail = test_fail pp_rule parser

let%test _ = parse_ok "a:b\n" { targets = "a", []; prerequisites = [ "b" ]; recipes = [] }

let%test _ =
  parse_ok
    "a \tb c: a"
    { targets = "a", [ "b"; "c" ]; prerequisites = [ "a" ]; recipes = [] }
;;

let%test _ =
  parse_ok
    "a:a \\\n #kek\n c\n"
    { targets = "a", []; prerequisites = [ "a"; "c" ]; recipes = [] }
;;

let%test _ =
  parse_ok
    "a:a \\\n b c\n"
    { targets = "a", []; prerequisites = [ "a"; "b"; "c" ]; recipes = [] }
;;

let%test _ =
  parse_ok
    "a:b\n#abc\n\n\trec1\n\trec2"
    { targets = "a", []; prerequisites = [ "b" ]; recipes = [ "rec1"; "rec2" ] }
;;

let%test _ =
  parse_ok
    "a:a \\\n #kek\n c\n\n   \t"
    { targets = "a", []; prerequisites = [ "a"; "c" ]; recipes = [] }
;;

let%test _ =
  parse_ok
    "a:a \\\n b c\n"
    { targets = "a", []; prerequisites = [ "a"; "b"; "c" ]; recipes = [] }
;;

let%test _ =
  parse_ok
    "a:a \\\n b c\n\tabc\n  \t"
    { targets = "a", []; prerequisites = [ "a"; "b"; "c" ]; recipes = [ "abc" ] }
;;

(* Parser that returns list of exprs (not quite ast, but close) *)
let rule_constructor t p r = Rule { targets = t; prerequisites = p; recipes = r }
let rule = lift3 rule_constructor targets prerequisites recipes
let parser = trim_start (many1 rule)
let parse_ok = test_ok (Format.pp_print_list pp_expr) parser
let parse_fail = test_fail (Format.pp_print_list pp_expr) parser

let%test _ =
  parse_ok
    "a:b\na:b\n"
    [ Rule { targets = "a", []; prerequisites = [ "b" ]; recipes = [] }
    ; Rule { targets = "a", []; prerequisites = [ "b" ]; recipes = [] }
    ]
;;

let%test _ =
  parse_ok
    "a:b\n   \n\n   \n  a:b\n"
    [ Rule { targets = "a", []; prerequisites = [ "b" ]; recipes = [] }
    ; Rule { targets = "a", []; prerequisites = [ "b" ]; recipes = [] }
    ]
;;

let%test _ =
  parse_ok
    {|a:b #	abc	  
	c|}
    [ Rule { targets = "a", []; prerequisites = [ "b" ]; recipes = [ "c" ] } ]
;;

let%test _ =
  parse_ok
    {|a   	:
	c|}
    [ Rule { targets = "a", []; prerequisites = []; recipes = [ "c" ] } ]
;;

let%test _ =
  parse_ok
    {|a:b
	
		a|}
    [ Rule { targets = "a", []; prerequisites = [ "b" ]; recipes = [ ""; "\ta" ] } ]
;;

let%test _ =
  parse_ok
    {|a:b
	
		a
     	c:d|}
    [ Rule { targets = "a", []; prerequisites = [ "b" ]; recipes = [ ""; "\ta" ] }
    ; Rule { targets = "c", []; prerequisites = [ "d" ]; recipes = [] }
    ]
;;

let%test _ =
  parse_ok
    {|   	#make comment
#comment    		
			  
         a   		 : 		b 	   
#make comment
     		
			echo a
   
    		
   		b    	:
	echo b
     
#kek|}
    [ Rule { targets = "a", []; prerequisites = [ "b" ]; recipes = [ "\t\techo a" ] }
    ; Rule { targets = "b", []; prerequisites = []; recipes = [ "echo b" ] }
    ]
;;

let%test _ =
  parse_ok
    {|some_file:
	echo "hello"
	touch some_file

file1 file3: header
	touch file1
	touch file3
file2: header
	touch file2

clean:
	rm -f file1 file2 file3 some_file|}
    [ Rule
        { targets = "some_file", []
        ; prerequisites = []
        ; recipes = [ "echo \"hello\""; "touch some_file" ]
        }
    ; Rule
        { targets = "file1", [ "file3" ]
        ; prerequisites = [ "header" ]
        ; recipes = [ "touch file1"; "touch file3" ]
        }
    ; Rule
        { targets = "file2", []
        ; prerequisites = [ "header" ]
        ; recipes = [ "touch file2" ]
        }
    ; Rule
        { targets = "clean", []
        ; prerequisites = []
        ; recipes = [ "rm -f file1 file2 file3 some_file" ]
        }
    ]
;;

let%test _ = parse_fail {|:b a|}
let%test _ = parse_fail {|a#:b a|}

(* ================================================= *)
(* Main parser                                       *)
(* Parses rules                                      *)
(* <target> [<target[s]>...]: [<prerequisite[s]>...] *)
(*  \t[<recipe[s]>...]                               *)
(* ================================================  *)
let parser =
  let rule =
    let rule_constructor t p r = { targets = t; prerequisites = p; recipes = r } in
    lift3 rule_constructor targets prerequisites recipes
  in
  let expr =
    let ast_constructor t p r = Rule { targets = t; prerequisites = p; recipes = r } in
    lift3 ast_constructor targets prerequisites recipes
  in
  trim_start (both rule (many expr))
;;

let parse_ok = test_ok pp_ast parser
let parse_fail = test_fail pp_ast parser

let%test _ =
  parse_ok
    "a:b\na:b\n"
    ( { targets = "a", []; prerequisites = [ "b" ]; recipes = [] }
    , [ Rule { targets = "a", []; prerequisites = [ "b" ]; recipes = [] } ] )
;;

let%test _ =
  parse_ok
    "a:b\n   \n\n   \n  a:b\n"
    ( { targets = "a", []; prerequisites = [ "b" ]; recipes = [] }
    , [ Rule { targets = "a", []; prerequisites = [ "b" ]; recipes = [] } ] )
;;

let%test _ =
  parse_ok "a   \t:" ({ targets = "a", []; prerequisites = []; recipes = [] }, [])
;;

let%test _ =
  parse_ok
    "a   \t:\n\tc"
    ({ targets = "a", []; prerequisites = []; recipes = [ "c" ] }, [])
;;

let%test _ =
  parse_ok
    {|a:b
	
		a|}
    ({ targets = "a", []; prerequisites = [ "b" ]; recipes = [ ""; "\ta" ] }, [])
;;

let%test _ =
  parse_ok
    {|a:b
	
		a
     	c:d|}
    ( { targets = "a", []; prerequisites = [ "b" ]; recipes = [ ""; "\ta" ] }
    , [ Rule { targets = "c", []; prerequisites = [ "d" ]; recipes = [] } ] )
;;

let%test _ =
  parse_ok
    {|   	#make comment
#comment    		
			  
         a   		 : 		b 	   
#make comment
     		
			echo a
   
    		
   		b    	:
	echo b
     
#kek|}
    ( { targets = "a", []; prerequisites = [ "b" ]; recipes = [ "\t\techo a" ] }
    , [ Rule { targets = "b", []; prerequisites = []; recipes = [ "echo b" ] } ] )
;;

let%test _ =
  parse_ok
    {|some_file:
	echo "hello"
	touch some_file
	#shell comment

#make comment
file1 file3: header \
             header2
	touch file1
	touch file3
file2: header
	touch file2

clean:
	rm -f file1 file2 file3 some_file|}
    ( { targets = "some_file", []
      ; prerequisites = []
      ; recipes = [ "echo \"hello\""; "touch some_file"; "#shell comment" ]
      }
    , [ Rule
          { targets = "file1", [ "file3" ]
          ; prerequisites = [ "header"; "header2" ]
          ; recipes = [ "touch file1"; "touch file3" ]
          }
      ; Rule
          { targets = "file2", []
          ; prerequisites = [ "header" ]
          ; recipes = [ "touch file2" ]
          }
      ; Rule
          { targets = "clean", []
          ; prerequisites = []
          ; recipes = [ "rm -f file1 file2 file3 some_file" ]
          }
      ] )
;;

let%test _ = parse_fail {|:b #	abc	  
	c|}

let%test _ = parse_fail {||}
let%test _ = parse_fail "a"
let%test _ = parse_fail "a::b\\\nc"
