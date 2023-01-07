(** Copyright 2021-2022, ol-imorozko and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

let is_whitespace = ( = ) ' '
let is_tab = ( = ) '\t'
let is_newline = ( = ) '\n'
let is_hash = ( = ) '#'
let is_colon = ( = ) ':'
let is_semicolon = ( = ) ';'
let is_percent = ( = ) '%'
let is_backslash = ( = ) '\\'
let is_regular_substitution_end = ( = ) ')'
let is_regular_substitution_start = ( = ) "$("
let is_asterisk_substitution = ( = ) "$*"
let is_at_substitution = ( = ) "$@"
let is_lesser_substitution = ( = ) "$<"
let is_recursive_assigment = ( = ) '='
let is_simply_assigment = ( = ) ":="
let is_conditional_assigment = ( = ) "?="
let not_newline = Fun.negate is_newline
let is_empty_char c = is_whitespace c || is_tab c || is_newline c
let some_pred preds el = List.exists (fun fn -> fn el) preds
let all_pred preds el = List.for_all (fun fn -> fn el) preds

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

(* Parses "words". Word is a string, containing symbols and substitutions,
 like "abc  $(x) - %". Here "$(x)" is a variable substitution, and "%" is a pattern *)
let word
  ?(parse_empty_characters = true)
  ?(parse_hash = false)
  ?(parse_equal_sign = true)
  ()
  =
  let string_p =
    let custom_take_while is_valid_str is_valid_char =
      (* fail if it's the end of the input or invalid symbol*)
      let impl peek pred =
        peek
        >>| pred
        >>= function
        | false -> fail "Invalid char"
        | true -> return true
      in
      let check_next_char = impl peek_char_fail is_valid_char in
      let check_next_string = impl (option "" (peek_string 2)) is_valid_str in
      many1 (check_next_char *> check_next_string *> take 1) >>| String.concat ""
    in
    let f _ = false in
    custom_take_while
      (Fun.negate
      @@ some_pred
           [ is_regular_substitution_start
           ; is_asterisk_substitution
           ; is_at_substitution
           ; is_lesser_substitution
           ; (if parse_equal_sign
             then f
             else some_pred [ is_simply_assigment; is_conditional_assigment ])
           ])
      (Fun.negate
      @@ some_pred
           [ is_newline
           ; (if parse_hash then f else is_hash)
           ; is_colon
           ; is_recursive_assigment
           ; is_semicolon
           ; is_regular_substitution_end
           ; is_backslash
           ; is_percent
           ; (if parse_empty_characters then f else some_pred [ is_whitespace; is_tab ])
           ])
  in
  let none_p = string_p >>| none in
  let pattern_p = char '%' >>| fun _ -> pattern in
  let asterisk_p = string "$*" >>| fun _ -> asterisk in
  let at_p = string "$@" >>| fun _ -> at in
  let lesser_p = string "$<" >>| fun _ -> lesser in
  fix (fun word ->
    let regular_p = string "$(" *> option [ None "" ] word <* char ')' >>| regular in
    many1 (none_p <|> regular_p <|> pattern_p <|> asterisk_p <|> at_p <|> lesser_p))
;;

let filename_delim =
  option () (some_pred [ is_whitespace; is_tab ] |> take_while1 |> discard)
;;

(* parse filenames separated by delimeters *)
let filenames = sep_and_trim filename_delim (word ~parse_empty_characters:false ())

(** Parse targets `<target> [<target[s]>...]:`
   "word := word" is not considered as a target, cause ':=' is a variable definiton
 *)
let targets =
  let check_assignment_and_parse_colon p_value =
    peek_char
    >>= function
    | Some ':' ->
      take 1 *> peek_char
      >>= (function
      | Some '=' -> fail "Got ':=', failing"
      | _ -> return p_value)
    | _ -> fail ": required"
  in
  both (word ~parse_empty_characters:false ()) filenames
  >>= check_assignment_and_parse_colon
;;

(* combine output from parser on multiline lines *)
let multiline p concat delim start =
  fix (fun g ->
    lift2
      concat
      p
      ((char '\\' <* filename_delim *> char '\n') *> g <|> delim *> return start))
;;

(**
 Parse prerequisites (could be multilined)
 We could not insert comments in between.
*)
let prerequisites =
  multiline (filename_delim *> many comment *> filenames) ( @ ) (many comment) []
;;

(**
 Parse recipes
 After targets:prerequisites parsing, recipes
 are new lines __starting with the tab__.
 However, if the same line starts with semicolon, it's also
 considered as a recipe.

 When a line starts with ‘@’, the echoing of that line is suppressed.
 The ‘@’ is discarded before the line is passed to the shell.
*)
let recipes =
  let empty_line =
    let ws_line = char ' ' *> filename_delim in
    wrap ws_line eols <|> discard (many1 end_of_line)
  in
  let recipe_delim = many (empty_line <|> comment) in
  let recipe =
    let str_p = option [ None "" ] (word ~parse_hash:true ()) in
    let echo_p = char '@' *> str_p >>| silent in
    let silent_p = str_p >>| echo in
    echo_p <|> silent_p
  in
  let first_recipe_line =
    option [] (char ';' *> trim_start recipe >>| fun str -> [ str ])
  in
  let one_recipe_line = char '\t' *> recipe in
  let concat a b =
    let word_of_recipe = function
      | Echo x -> x
      | Silent x -> x
    in
    match a with
    | Echo x -> x @ word_of_recipe b |> echo
    | Silent x -> x @ word_of_recipe b |> silent
  in
  let recipe_line = multiline one_recipe_line concat recipe_delim (echo []) in
  lift2 ( @ ) first_recipe_line (sep_and_trim recipe_delim recipe_line)
;;

(* Parses variable definiton. name [?, :]<=> value *)
let variable =
  let name =
    wrap (word ~parse_empty_characters:false ~parse_equal_sign:false ()) filename_delim
  in
  let value = prerequisites in
  let cons f (n, v) = f n v in
  let recursive_p = both (name <* char '=') value >>| cons recursive in
  let simply_p = both (name <* string ":=") value >>| cons simply in
  let conditional_p = both (name <* string "?=") value >>| cons conditional in
  recursive_p <|> simply_p <|> conditional_p
;;

let rule_constructor t p r = { targets = t; prerequisites = p; recipes = r }

(* Parses rule
 <target> [<target[s]>...]: [<prerequisite[s]>...]
  \t[<recipe[s]>...]
 *)
let rule = lift3 rule_constructor targets prerequisites recipes

(* Parses make expression
  Expressions are rules and variable definitions.
 *)
let expr =
  let expr_variable = lift (fun var -> Var var) variable in
  let expr_rule = lift (fun rule -> Rule rule) rule in
  expr_rule <|> expr_variable
;;

(**
 Main parser
 Parses at least one rule and any number of make expressions.
 Comments and newlines in between are skipped
*)
let make =
  let custom_many p = sep_and_trim skip_meaningless_characters p in
  lift3
    (fun lhs rule rhs -> (rule, List.length lhs), lhs @ rhs)
    (custom_many
    @@ (option Option.None (rule >>= fun x -> return (Some x))
       >>= function
       | Some _ -> fail "found rule"
       | None -> expr))
    rule
    (custom_many expr)
;;

let parse str = parse_string ~consume:Consume.All make str

(* ------------------------------------------------- *)
(* ----------------------TESTS---------------------- *)
(* ------------------------------------------------- *)

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

(* trim parser test *)
let tmp = { targets = [ None "abc" ], []; prerequisites = []; recipes = [] }
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
let%test _ = parse_fail "#"
let%test _ = parse_fail "    #fodsfjo \r#x  \n# f"
let%test _ = parse_fail "#fodsfjo\n \t \t \n "

(* word parser test *)
let parse_ok = test_ok pp_word (word ())
let parse_fail = test_fail pp_word (word ())

let%test _ = parse_ok "a" [ None "a" ]
let%test _ = parse_ok "$(a)" [ Regular [ None "a" ] ]
let%test _ = parse_ok "$()" [ Regular [ None "" ] ]
let%test _ = parse_ok "%" [ Pattern ]
let%test _ = parse_ok "$*" [ Asterisk ]
let%test _ = parse_ok "$@" [ At ]
let%test _ = parse_ok "$<" [ Lesser ]
let%test _ = parse_ok "a $(  a)" [ None "a "; Regular [ None "  a" ] ]
let%test _ = parse_ok "$(%)" [ Regular [ Pattern ] ]
let%test _ = parse_ok "$($*)" [ Regular [ Asterisk ] ]
let%test _ = parse_ok "$($<)" [ Regular [ Lesser ] ]
let%test _ = parse_ok "$($($(x)))" [ Regular [ Regular [ Regular [ None "x" ] ] ] ]

let%test _ =
  parse_ok
    "a$(A)  $*__+$< $@%"
    [ None "a"
    ; Regular [ None "A" ]
    ; None "  "
    ; Asterisk
    ; None "__+"
    ; Lesser
    ; None " "
    ; At
    ; Pattern
    ]
;;

let%test _ = parse_fail ""
let%test _ = parse_fail "$("
let%test _ = parse_fail "aaa$(aaa"

(* targets parser test *)
type targets = word * word list [@@deriving show { with_path = false }]

let parser = targets
let parse_ok = test_ok pp_targets parser
let parse_fail = test_fail pp_targets parser

let%test _ = parse_ok "abc:" ([ None "abc" ], [])
let%test _ = parse_ok "abc  :" ([ None "abc" ], [])
let%test _ = parse_ok "abc \t  :" ([ None "abc" ], [])
let%test _ = parse_ok "a  \t:" ([ None "a" ], [])
let%test _ = parse_ok "a\tb:" ([ None "a" ], [ [ None "b" ] ])
let%test _ = parse_ok "a \t b c,dex :" ([ None "a" ], [ [ None "b" ]; [ None "c,dex" ] ])
let%test _ = parse_ok "abc \t f  f :" ([ None "abc" ], [ [ None "f" ]; [ None "f" ] ])
let%test _ = parse_ok "abc \t f  f :" ([ None "abc" ], [ [ None "f" ]; [ None "f" ] ])

let%test _ =
  parse_ok
    "$(a) \t %%% abc :"
    ([ Regular [ None "a" ] ], [ [ Pattern; Pattern; Pattern ]; [ None "abc" ] ])
;;

let%test _ = parse_fail "abc\n:"
let%test _ = parse_fail "abc#:"
let%test _ = parse_fail ":#c:"
let%test _ = parse_fail ":"
let%test _ = parse_fail "a:=b"
let%test _ = parse_fail ":::"
let%test _ = parse_fail ""
let%test _ = parse_fail ":  \t fdsf \n"

(* prerequisites parser test *)
let parser = prerequisites
let parse_ok = test_ok (Format.pp_print_list pp_word) parser
let parse_fail = test_fail (Format.pp_print_list pp_word) parser

let%test _ = parse_ok "abc" [ [ None "abc" ] ]
let%test _ = parse_ok "abc  " [ [ None "abc" ] ]
let%test _ = parse_ok "    abc" [ [ None "abc" ] ]
let%test _ = parse_ok "abc \t  " [ [ None "abc" ] ]
let%test _ = parse_ok "a \t b c,dex" [ [ None "a" ]; [ None "b" ]; [ None "c,dex" ] ]
let%test _ = parse_ok "abc \t f  f " [ [ None "abc" ]; [ None "f" ]; [ None "f" ] ]
let%test _ = parse_ok "abc #\t f  f \t" [ [ None "abc" ] ]
let%test _ = parse_ok "abc #\n" [ [ None "abc" ] ]
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
let%test _ = parse_ok "a\\\nb" [ [ None "a" ]; [ None "b" ] ]
let%test _ = parse_ok " \\\n\ta" [ [ None "a" ] ]
let%test _ = parse_ok "  \\  \t \n  a" [ [ None "a" ] ]
let%test _ = parse_ok "  \\\n  a" [ [ None "a" ] ]
let%test _ = parse_ok "a\\\n  \t  \t " [ [ None "a" ] ]
let%test _ = parse_ok "a\\\n  \t  \t b  " [ [ None "a" ]; [ None "b" ] ]

let%test _ =
  parse_ok "a$(x)\\\n  \t  \t %  " [ [ None "a"; Regular [ None "x" ] ]; [ Pattern ] ]
;;

let%test _ = parse_ok "a  \\\n  b" [ [ None "a" ]; [ None "b" ] ]
let%test _ = parse_ok "a\\\n" [ [ None "a" ] ]

let%test _ =
  parse_ok
    "a \\\n b c \\\n d e"
    [ [ None "a" ]; [ None "b" ]; [ None "c" ]; [ None "d" ]; [ None "e" ] ]
;;

let%test _ = parse_ok "a\\\n" [ [ None "a" ] ]
let%test _ = parse_ok "a#\\\n" [ [ None "a" ] ]
let%test _ = parse_fail "a\\#b\n"
let%test _ = parse_fail "a\\#b\n"
let%test _ = parse_fail "a\na"
let%test _ = parse_fail "a\\\n\na"
let%test _ = parse_fail "a\\\n\n  abc:"
let%test _ = parse_fail "a\\\n  b:"

(* combined targets;prerequisites parser test *)
type rule_wo_recepie =
  { targets : word * word list
  ; prerequisites : word list
  }
[@@deriving show { with_path = false }]

let parser = lift2 (fun t p -> { targets = t; prerequisites = p }) targets prerequisites
let parse_ok = test_ok pp_rule_wo_recepie parser
let parse_fail = test_fail pp_rule_wo_recepie parser

let%test _ =
  parse_ok "a:b" { targets = [ None "a" ], []; prerequisites = [ [ None "b" ] ] }
;;

let%test _ =
  parse_ok
    "a \tb c: a"
    { targets = [ None "a" ], [ [ None "b" ]; [ None "c" ] ]
    ; prerequisites = [ [ None "a" ] ]
    }
;;

let%test _ =
  parse_ok
    "$(a) \t%$(b) %x : % $(x)"
    { targets =
        ( [ Regular [ None "a" ] ]
        , [ [ Pattern; Regular [ None "b" ] ]; [ Pattern; None "x" ] ] )
    ; prerequisites = [ [ Pattern ]; [ Regular [ None "x" ] ] ]
    }
;;

let%test _ =
  parse_ok
    "a:a \\\n #kek\n c \t"
    { targets = [ None "a" ], []; prerequisites = [ [ None "a" ]; [ None "c" ] ] }
;;

let%test _ =
  parse_ok
    "a:a \\\n b c"
    { targets = [ None "a" ], []
    ; prerequisites = [ [ None "a" ]; [ None "b" ]; [ None "c" ] ]
    }
;;

let%test _ = parse_fail "a:b\n"
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

(* recipes parser test *)
let parser = recipes
let parse_ok = test_ok (Format.pp_print_list pp_recipe) parser
let parse_fail = test_fail (Format.pp_print_list pp_recipe) parser

let%test _ = parse_ok "\tabc" [ Echo [ None "abc" ] ]
let%test _ = parse_ok "\tabc\n" [ Echo [ None "abc" ] ]
let%test _ = parse_ok "\t\t\tabc\n" [ Echo [ None "\t\tabc" ] ]
let%test _ = parse_ok "\tabc\n  #abcde\n" [ Echo [ None "abc" ] ]
let%test _ = parse_ok "\tabc\n#x\n" [ Echo [ None "abc" ] ]
let%test _ = parse_ok "\n   \n\t   abc\t\t#abc" [ Echo [ None "   abc\t\t#abc" ] ]
let%test _ = parse_ok "\n   \n\t#comment\n" [ Echo [ None "#comment" ] ]
let%test _ = parse_ok "#cmnt\n\n\tabc\n" [ Echo [ None "abc" ] ]

let%test _ =
  parse_ok "\ta\n\tb\n\tc\n  " [ Echo [ None "a" ]; Echo [ None "b" ]; Echo [ None "c" ] ]
;;

let%test _ = parse_ok "\n\n\n    \n" []
let%test _ = parse_ok "\n   \t\n\ta" [ Echo [ None "a" ] ]
let%test _ = parse_fail "\n#abc\n  not a recipe cause not \t \n"

(* combined targets;prerequisites;recipes parser test *)
let rule_constructor t p r = { targets = t; prerequisites = p; recipes = r }
let parser = lift3 rule_constructor targets prerequisites recipes
let parse_ok = test_ok pp_rule parser
let parse_fail = test_fail pp_rule parser

let%test _ =
  parse_ok
    "a:b\n"
    { targets = [ None "a" ], []; prerequisites = [ [ None "b" ] ]; recipes = [] }
;;

let%test _ =
  parse_ok
    "a \tb c: a"
    { targets = [ None "a" ], [ [ None "b" ]; [ None "c" ] ]
    ; prerequisites = [ [ None "a" ] ]
    ; recipes = []
    }
;;

let%test _ =
  parse_ok
    "a: a\n\t echo abc\\\n\tcde"
    { targets = [ None "a" ], []
    ; prerequisites = [ [ None "a" ] ]
    ; recipes = [ Echo [ None " echo abc"; None "cde" ] ]
    }
;;

let%test _ =
  parse_ok
    "a: a\n\t echo a$(a)\\\n\t$(b)c"
    { targets = [ None "a" ], []
    ; prerequisites = [ [ None "a" ] ]
    ; recipes =
        [ Echo [ None " echo a"; Regular [ None "a" ]; Regular [ None "b" ]; None "c" ] ]
    }
;;

let%test _ =
  parse_ok
    "a: a\n\t@echo abc\\\n\tcde"
    { targets = [ None "a" ], []
    ; prerequisites = [ [ None "a" ] ]
    ; recipes = [ Silent [ None "echo abc"; None "cde" ] ]
    }
;;

let%test _ =
  parse_ok
    "a: a\n\tabc  \\\n\tcde  \\\n\tefg"
    { targets = [ None "a" ], []
    ; prerequisites = [ [ None "a" ] ]
    ; recipes = [ Echo [ None "abc  "; None "cde  "; None "efg" ] ]
    }
;;

let%test _ =
  parse_ok
    "a:a \\\n #kek\n c\n"
    { targets = [ None "a" ], []
    ; prerequisites = [ [ None "a" ]; [ None "c" ] ]
    ; recipes = []
    }
;;

let%test _ =
  parse_ok
    "a:a \\\n b c\n"
    { targets = [ None "a" ], []
    ; prerequisites = [ [ None "a" ]; [ None "b" ]; [ None "c" ] ]
    ; recipes = []
    }
;;

let%test _ =
  parse_ok
    "a:b\n#abc\n\n\trec1\n\trec2"
    { targets = [ None "a" ], []
    ; prerequisites = [ [ None "b" ] ]
    ; recipes = [ Echo [ None "rec1" ]; Echo [ None "rec2" ] ]
    }
;;

let%test _ =
  parse_ok
    "a:a \\\n #kek\n c\n\n   \t"
    { targets = [ None "a" ], []
    ; prerequisites = [ [ None "a" ]; [ None "c" ] ]
    ; recipes = []
    }
;;

let%test _ =
  parse_ok
    "a:a \\\n b c\n"
    { targets = [ None "a" ], []
    ; prerequisites = [ [ None "a" ]; [ None "b" ]; [ None "c" ] ]
    ; recipes = []
    }
;;

let%test _ =
  parse_ok
    "a:a \\\n b c\n\tabc\n  \t"
    { targets = [ None "a" ], []
    ; prerequisites = [ [ None "a" ]; [ None "b" ]; [ None "c" ] ]
    ; recipes = [ Echo [ None "abc" ] ]
    }
;;

let%test _ =
  parse_ok
    "a : b; abc"
    { targets = [ None "a" ], []
    ; prerequisites = [ [ None "b" ] ]
    ; recipes = [ Echo [ None "abc" ] ]
    }
;;

let%test _ =
  parse_ok
    "a : b\n\t abc\n\t@abc\\\n\tcde"
    { targets = [ None "a" ], []
    ; prerequisites = [ [ None "b" ] ]
    ; recipes = [ Echo [ None " abc" ]; Silent [ None "abc"; None "cde" ] ]
    }
;;

let%test _ =
  parse_ok
    "a : b; abc\n\t kek\\\n\tkek"
    { targets = [ None "a" ], []
    ; prerequisites = [ [ None "b" ] ]
    ; recipes = [ Echo [ None "abc" ]; Echo [ None " kek"; None "kek" ] ]
    }
;;

(* combined targets;prerequisites;recipes parser test that returns list of exprs test *)
let rule_constructor t p r = Rule { targets = t; prerequisites = p; recipes = r }
let rule = lift3 rule_constructor targets prerequisites recipes
let parser = trim_start (many1 rule)
let parse_ok = test_ok (Format.pp_print_list pp_expr) parser
let parse_fail = test_fail (Format.pp_print_list pp_expr) parser

let%test _ =
  parse_ok
    "a:b\na:b\n"
    [ Rule { targets = [ None "a" ], []; prerequisites = [ [ None "b" ] ]; recipes = [] }
    ; Rule { targets = [ None "a" ], []; prerequisites = [ [ None "b" ] ]; recipes = [] }
    ]
;;

let%test _ =
  parse_ok
    "a:b\n   \n\n   \n  a:b\n"
    [ Rule { targets = [ None "a" ], []; prerequisites = [ [ None "b" ] ]; recipes = [] }
    ; Rule { targets = [ None "a" ], []; prerequisites = [ [ None "b" ] ]; recipes = [] }
    ]
;;

let%test _ =
  parse_ok
    {|a:b #	abc	  
	c|}
    [ Rule
        { targets = [ None "a" ], []
        ; prerequisites = [ [ None "b" ] ]
        ; recipes = [ Echo [ None "c" ] ]
        }
    ]
;;

let%test _ =
  parse_ok
    {|a   	:
	c|}
    [ Rule
        { targets = [ None "a" ], []
        ; prerequisites = []
        ; recipes = [ Echo [ None "c" ] ]
        }
    ]
;;

let%test _ =
  parse_ok
    {|a:b
	
		a|}
    [ Rule
        { targets = [ None "a" ], []
        ; prerequisites = [ [ None "b" ] ]
        ; recipes = [ Echo [ None "" ]; Echo [ None "\ta" ] ]
        }
    ]
;;

let%test _ =
  parse_ok
    {|a:b
	
		a
     	c:d|}
    [ Rule
        { targets = [ None "a" ], []
        ; prerequisites = [ [ None "b" ] ]
        ; recipes = [ Echo [ None "" ]; Echo [ None "\ta" ] ]
        }
    ; Rule { targets = [ None "c" ], []; prerequisites = [ [ None "d" ] ]; recipes = [] }
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
    [ Rule
        { targets = [ None "a" ], []
        ; prerequisites = [ [ None "b" ] ]
        ; recipes = [ Echo [ None "\t\techo a" ] ]
        }
    ; Rule
        { targets = [ None "b" ], []
        ; prerequisites = []
        ; recipes = [ Echo [ None "echo b" ] ]
        }
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
        { targets = [ None "some_file" ], []
        ; prerequisites = []
        ; recipes = [ Echo [ None "echo \"hello\"" ]; Echo [ None "touch some_file" ] ]
        }
    ; Rule
        { targets = [ None "file1" ], [ [ None "file3" ] ]
        ; prerequisites = [ [ None "header" ] ]
        ; recipes = [ Echo [ None "touch file1" ]; Echo [ None "touch file3" ] ]
        }
    ; Rule
        { targets = [ None "file2" ], []
        ; prerequisites = [ [ None "header" ] ]
        ; recipes = [ Echo [ None "touch file2" ] ]
        }
    ; Rule
        { targets = [ None "clean" ], []
        ; prerequisites = []
        ; recipes = [ Echo [ None "rm -f file1 file2 file3 some_file" ] ]
        }
    ]
;;

let%test _ = parse_fail {|:b a|}
let%test _ = parse_fail {|a#:b a|}

(* Multiple rules parsing test *)
let parser = make
let parse_ok = test_ok pp_ast parser
let parse_fail = test_fail pp_ast parser

let%test _ =
  parse_ok
    "a:b\na:b\n"
    ( ({ targets = [ None "a" ], []; prerequisites = [ [ None "b" ] ]; recipes = [] }, 0)
    , [ Rule
          { targets = [ None "a" ], []; prerequisites = [ [ None "b" ] ]; recipes = [] }
      ] )
;;

let%test _ =
  parse_ok
    "a:b\n   \n\n   \n  a:b\n"
    ( ({ targets = [ None "a" ], []; prerequisites = [ [ None "b" ] ]; recipes = [] }, 0)
    , [ Rule
          { targets = [ None "a" ], []; prerequisites = [ [ None "b" ] ]; recipes = [] }
      ] )
;;

let%test _ =
  parse_ok
    "a   \t:"
    (({ targets = [ None "a" ], []; prerequisites = []; recipes = [] }, 0), [])
;;

let%test _ =
  parse_ok
    "a   \t:\n\tc"
    ( ( { targets = [ None "a" ], []; prerequisites = []; recipes = [ Echo [ None "c" ] ] }
      , 0 )
    , [] )
;;

let%test _ =
  parse_ok
    {|a:b
	
		a|}
    ( ( { targets = [ None "a" ], []
        ; prerequisites = [ [ None "b" ] ]
        ; recipes = [ Echo [ None "" ]; Echo [ None "\ta" ] ]
        }
      , 0 )
    , [] )
;;

let%test _ =
  parse_ok
    {|a:b
	
		a
     	c:d|}
    ( ( { targets = [ None "a" ], []
        ; prerequisites = [ [ None "b" ] ]
        ; recipes = [ Echo [ None "" ]; Echo [ None "\ta" ] ]
        }
      , 0 )
    , [ Rule
          { targets = [ None "c" ], []; prerequisites = [ [ None "d" ] ]; recipes = [] }
      ] )
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
    ( ( { targets = [ None "a" ], []
        ; prerequisites = [ [ None "b" ] ]
        ; recipes = [ Echo [ None "\t\techo a" ] ]
        }
      , 0 )
    , [ Rule
          { targets = [ None "b" ], []
          ; prerequisites = []
          ; recipes = [ Echo [ None "echo b" ] ]
          }
      ] )
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
    ( ( { targets = [ None "some_file" ], []
        ; prerequisites = []
        ; recipes =
            [ Echo [ None "echo \"hello\"" ]
            ; Echo [ None "touch some_file" ]
            ; Echo [ None "#shell comment" ]
            ]
        }
      , 0 )
    , [ Rule
          { targets = [ None "file1" ], [ [ None "file3" ] ]
          ; prerequisites = [ [ None "header" ]; [ None "header2" ] ]
          ; recipes = [ Echo [ None "touch file1" ]; Echo [ None "touch file3" ] ]
          }
      ; Rule
          { targets = [ None "file2" ], []
          ; prerequisites = [ [ None "header" ] ]
          ; recipes = [ Echo [ None "touch file2" ] ]
          }
      ; Rule
          { targets = [ None "clean" ], []
          ; prerequisites = []
          ; recipes = [ Echo [ None "rm -f file1 file2 file3 some_file" ] ]
          }
      ] )
;;

let%test _ =
  parse_ok
    {|$(%) : $*
	@echo "$(a)+%\
	ff"
|}
    ( ( { targets = [ Regular [ Pattern ] ], []
        ; prerequisites = [ [ Asterisk ] ]
        ; recipes =
            [ Silent
                [ None "echo \""; Regular [ None "a" ]; None "+"; Pattern; None "ff\"" ]
            ]
        }
      , 0 )
    , [] )
;;

(*Variable tests*)
let%test _ =
  parse_ok
    "a=b\n  \n\t\t\n   \n a:b #fdsf  \n   x:y   \n   a=b"
    ( ({ targets = [ None "a" ], []; prerequisites = [ [ None "b" ] ]; recipes = [] }, 1)
    , [ Var (Recursive ([ None "a" ], [ [ None "b" ] ]))
      ; Rule
          { targets = [ None "x" ], []; prerequisites = [ [ None "y" ] ]; recipes = [] }
      ; Var (Recursive ([ None "a" ], [ [ None "b" ] ]))
      ] )
;;

let%test _ =
  parse_ok
    "a=b\n  \n\t\t\n   \n a:b #fdsf  \n   x:y   \n   a=b"
    ( ({ targets = [ None "a" ], []; prerequisites = [ [ None "b" ] ]; recipes = [] }, 1)
    , [ Var (Recursive ([ None "a" ], [ [ None "b" ] ]))
      ; Rule
          { targets = [ None "x" ], []; prerequisites = [ [ None "y" ] ]; recipes = [] }
      ; Var (Recursive ([ None "a" ], [ [ None "b" ] ]))
      ] )
;;

let%test _ =
  parse_ok
    "a:=b\n  $(a):=$(b)c $(x$(y))\n a:b"
    ( ({ targets = [ None "a" ], []; prerequisites = [ [ None "b" ] ]; recipes = [] }, 2)
    , [ Var (Simply ([ None "a" ], [ [ None "b" ] ]))
      ; Var
          (Simply
             ( [ Regular [ None "a" ] ]
             , [ [ Regular [ None "b" ]; None "c" ]
               ; [ Regular [ None "x"; Regular [ None "y" ] ] ]
               ] ))
      ] )
;;

let%test _ =
  parse_ok
    "a:b\n $($(x))y ?= $(y)x   \n"
    ( ({ targets = [ None "a" ], []; prerequisites = [ [ None "b" ] ]; recipes = [] }, 0)
    , [ Var
          (Conditional
             ( [ Regular [ Regular [ None "x" ] ]; None "y" ]
             , [ [ Regular [ None "y" ]; None "x" ] ] ))
      ] )
;;

let%test _ = parse_fail {|:b #	abc	  
	c|}

let%test _ = parse_fail {||}
let%test _ = parse_fail "a"
let%test _ = parse_fail "a:b\n a = b = c"
let%test _ = parse_fail "a::b\\\nc"
