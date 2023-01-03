(** Copyright 2021-2022, Chizhov Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

(* Helper functions *)

(* is space or tab *)
let is_space = function
  | ' ' | '\t' -> true
  | _ -> false
;;

let spaces = skip_while is_space

(* is newline or carriage return character *)
let is_delim = function
  | '\n' | '\r' -> true
  | _ -> false
;;

(* skip delims and spaces *)
let delim_spaces = skip_while (Utils.some_pred [ is_space; is_delim ])

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

(* is digit or alpha or _ *)
let is_char = Utils.some_pred [ is_digit; is_alpha; ( = ) '_' ]
let word = take_while1 is_char

let is_meta = function
  | '|' | '&' | ';' | '(' | ')' | '<' | '>' -> true
  | c when Utils.some_pred [ is_space; is_delim ] c -> true
  | _ -> false
;;

(* take semicolon *)
let sc = char ';'

(* take dollar  *)
let dollar = char '$'

(* take unsigned int *)
let u_int = take_while1 is_digit >>| int_of_string

(* take signed int *)
let int =
  let sign = option "" (string "-" <|> string "+") in
  let digits = take_while1 is_digit in
  lift2 (fun s u_i -> int_of_string (s ^ u_i)) sign digits
;;

(* modified take function, that can receive predicate as optional labeled param *)
let take ?(pred = Fun.const true) i =
  take i
  >>= function
  | x when String.for_all pred x -> return x
  | _ -> fail "Expected literal value"
;;

(* function, that can receive predicate as optional labeled param with *)
let take_ch ~pred = take ~pred 1 >>| fun x -> x.[0]

(* take word while not end_char or meta symbol *)
let take_while_not ?(end_chars = [ '}' ]) () =
  take_while1
    (Utils.all_pred [ (fun c -> List.for_all (( != ) c) end_chars); Fun.negate is_meta ])
;;

(* names cannot start by digit *)
let is_valid_name = function
  | s when is_digit (String.get s 0) -> fail "Variable name cannot start with digit"
  | s -> return s
;;

let name = word >>= is_valid_name

let var_p =
  lift2
    (fun name subscript -> { name; subscript })
    name
    (option "0" (char '[' *> take_till (( = ) ']') <* char ']'))
;;

(* trim left spaces *)
let trim_l p = spaces *> p

(* trim right spaces *)
let trim_r p = p <* spaces

(* trim left and right spaces *)
let trim p = trim_l @@ trim_r p

(* trim quotes around p *)
let quotes ~ch p = char ch *> p <* char ch

(* trim parens around p *)
let parens p = trim @@ (char '(' *> p <* char ')')

(* trim double parens around p *)
let parens2 p = parens (parens p)

(* trim braces around p *)
let braces p = char '{' *> p <* char '}'

(* trim backticks around p *)
let backticks p = char '`' *> p <* char '`'

(* skip spaces and tab chars *)
let skip_delim = skip_while is_space

(* choice newline, sc, space or tab *)
let delim_space_sc = choice [ char '\n'; sc; char ' '; char '\t' ]
let delim_spaces_sc = many delim_space_sc
let delim_spaces_sc1 = many1 delim_space_sc

(* skip delim spaces around p (at least 1 on either side) *)
let delim_spaces_around_sc1 p =
  delim_spaces_sc1 *> p <* delim_spaces_sc <|> (delim_spaces_sc *> p <* delim_spaces_sc1)
;;

(* skip delim spaces aroud optional single semicolon *)
let spaces_delims_around_sc = delim_spaces <* option ' ' sc <* delim_spaces

(* Tries to parse with the first parser, otherwise with the second.
   The result is stored in the first and second array, respectively.
   Then tries to parse separator and repeats first step as long as it can use
   one of the parsers. *)
let or_parse_many1 s p1 p2 =
  fix (fun m ->
    lift2
      (fun (lhs, rhs) (l1, l2) -> List.append lhs l1, List.append rhs l2)
      (p1 >>| (fun e -> [ e ], []) <|> (p2 >>| fun e -> [], [ e ]))
      (s *> m <|> return ([], [])))
;;

let or_parse_many s p1 p2 = option ([], []) (or_parse_many1 s p1 p2)

(* Binary operators *)

let add = char '+' *> return plus
let sub = char '-' *> return minus
let mul = char '*' *> return asterisk
let div = char '/' *> return slash

(* Compare operators (Predicates) *)
let greater = string ">" *> return greater
let greater_or_equal = string ">=" *> return greaterorequal
let less = string "<" *> return less
let less_or_equal = string "<=" *> return lessorequal
let equal = string "=" *> return equal
let not_equal = string "!=" *> return notequal

let operators_order =
  [ equal <|> not_equal
  ; greater_or_equal <|> greater <|> less_or_equal <|> less
  ; add <|> sub
  ; div <|> mul
  ]
;;

(* Context, which is needed in order to restrict the set
   of rules that can be applied in a particular context *)
type arg_context =
  | Default
  | InnerAS
  | InnerSQS
  | InnerDQS
  | InnerCommandSubstitution
[@@deriving show { with_path = false }]

type transformation_type =
  | WithoutQuotes
  | SingleQuotes
  | DoubleQuotes
  | BraceExpansion
  | ParamExpansion
  | CommandSubstitution
  | Arithmetic
[@@deriving show { with_path = false }]

type context =
  { arg_ctx : arg_context
  ; allow : transformation_type list
  }
[@@deriving fields, show { with_path = false }]

let full_context_by arg_ctx =
  let all_transformations =
    [ SingleQuotes
    ; DoubleQuotes
    ; BraceExpansion
    ; ParamExpansion
    ; Arithmetic
    ; CommandSubstitution
    ; WithoutQuotes
    ]
  in
  let except t = Utils.diff all_transformations t in
  let allow =
    match arg_ctx with
    | Default | InnerAS -> all_transformations
    | InnerSQS -> except all_transformations
    | InnerDQS -> except [ SingleQuotes; DoubleQuotes; BraceExpansion ]
    | InnerCommandSubstitution -> all_transformations
  in
  { arg_ctx; allow }
;;

let skip = fail "Skip (there are no suitable transformations)"

(* processes backslashes in different contexts *)
let backslash arg_ctx =
  let lift p = lift2 p (string "\\") (take 1) in
  match arg_ctx with
  | InnerDQS ->
    lift (fun slash -> function
      | ch when List.mem ch [ "$"; "'"; "\""; "\\"; "\n" ] -> ch
      | "\n" -> ""
      | ch -> slash ^ ch)
  | InnerAS -> lift (fun _ ch -> if ch = "\n" then "" else ch)
  | InnerSQS -> fail "Backslashes in single quoted string are treated as common char"
  | _ -> skip
;;

(* parses string in different contexts *)
let text_p ~arg_ctx =
  (match arg_ctx with
   | InnerSQS -> [ "\'" ]
   | InnerDQS -> [ "\""; "$" ]
   | InnerAS ->
     [ ","
     ; "!"
     ; "`"
     ; "$"
     ; "\n"
     ; ";"
     ; " "
     ; "\'"
     ; "\""
     ; "("
     ; ")"
     ; "|"
     ; "&"
     ; "}"
     ; "{"
     ; ">"
     ; "<"
     ; "&"
     ]
   | _ -> [])
  |> fun stop_chars ->
  let char_p = function
    | ch when List.mem ch stop_chars -> fail "Received stop character"
    | ch -> return ch
  in
  many1 (backslash arg_ctx <|> (take 1 >>= char_p)) >>| String.concat ""
;;

let expr_p =
  let chainl1 op e =
    let rec go acc = lift2 (fun f -> f acc) op e >>= go <|> return acc in
    e >>= go
  in
  let create pos = { name = string_of_int pos; subscript = "0" } in
  let var = trim (var_p <|> (dollar *> u_int >>| create)) >>| variable in
  let num = trim int >>| number in
  let assn = lift2 assignment (trim var_p <* char '=') in
  fix (fun expr ->
    let factor = parens expr <|> num <|> var in
    assn expr <|> List.fold_right chainl1 operators_order factor)
;;

(**      Param expansion parsers      *)

(* length expansion *)
let length = lift length (char '#' *> var_p)

(* substring expansion *)
let substring =
  let create name offset length =
    substring @@ Fields_of_substring.create ~name ~offset ~length
  in
  lift3
    create
    (trim_r var_p)
    (char ':' *> trim expr_p)
    (option None (char ':' *> trim expr_p >>| Option.some))
;;

(* substring removal expansion *)
let substr_removal =
  let create name ch pattern =
    Fields_of_substr_removal.create
      ~name
      ~pattern
      ~from:(if String.starts_with ~prefix:"#" ch then frombegin else fromend)
      ~min_or_max:(if String.length ch = 1 then min else max)
  in
  lift3
    create
    var_p
    (string "#" <|> string "%" >>= fun ch -> option "" (string ch) >>| ( ^ ) ch)
    (take_while_not ())
  |> lift substrremoval
;;

(* substitute expansion *)
let substitute =
  let create name subst_type pattern by =
    Fields_of_substitute.create ~name ~pattern ~by ~subst_type
  in
  let resolve_arr = [ "//", all; "/#", first; "/%", last; "/", one ] in
  let subst_type_p =
    choice @@ List.map (fun (s, fn) -> string s *> return fn) resolve_arr
  in
  lift4
    create
    var_p
    subst_type_p
    (take_while_not ~end_chars:[ '}'; '/' ] ())
    (option "" (char '/' *> take_while_not ()))
  |> lift substitute
;;

(* main parameter expansion function *)
let param_exp =
  let positional = braces u_int <|> (take ~pred:is_digit 1 >>| int_of_string) in
  let var_exp = lift varexpansion var_p in
  let expansions = [ length; substring; substr_removal; substitute; var_exp ] in
  lift
    paramexpansion
    (dollar
    *> choice
         [ name >>| varnameexpansion
         ; positional >>| positionalparam
         ; braces (choice expansions)
         ])
;;

(* helper type for literal and integer brace expansion *)
type 'a sequence_expansion =
  { seq_begin : 'a
  ; seq_end : 'a
  ; incr : int
  }
[@@deriving fields, show { with_path = false }]

(* this type is a list element inside curly braces
   in string brace expansion.
   CommaSep means comma separated argument
*)
type str_brace_exp_el = CommaSep of simple_string list
[@@deriving variants, show { with_path = false }]

type braced_expansion =
  | StringBraceExp of str_brace_exp_el list (* [ {[element [,]]...} ] *)
  | LiteralBraceExp of char sequence_expansion (* {char..char[..incr]} *)
  | IntBraceExp of int sequence_expansion (* {int..int[..incr]} *)
[@@deriving variants, show { with_path = false }]

type braced_expansion_or_simple_string =
  | BracedExpansion of braced_expansion
  | SimpleString of simple_string
[@@deriving variants, show { with_path = false }]

(* parses inner quoted string or string without quotes.*)
let rec atom_string ~arg_ctx =
  let { allow } = full_context_by arg_ctx in
  let resolver = function
    | ParamExpansion -> param_exp
    | WithoutQuotes | SingleQuotes | DoubleQuotes -> lift text @@ text_p ~arg_ctx
    | Arithmetic -> lift arithmexpansion (dollar *> parens2 expr_p)
    | CommandSubstitution -> command_substitution ()
    | _ -> skip
  in
  choice (List.map resolver allow)

(* Command substitution parser *)
and command_substitution () =
  let pipe = pipe ~arg_ctx:InnerCommandSubstitution () in
  lift commandsubstitution (dollar *> parens pipe <|> backticks pipe)

(* parses simple string (quoted string or string without quotes) *)
and simple_string ~arg_ctx () =
  let { allow } = full_context_by arg_ctx in
  let resolver = function
    | SingleQuotes ->
      lift singlequotedstring @@ quotes ~ch:'\'' @@ text_p ~arg_ctx:InnerSQS
    | DoubleQuotes ->
      lift doublequotedstring @@ quotes ~ch:'\"' @@ many1 @@ atom_string ~arg_ctx:InnerDQS
    | WithoutQuotes -> lift atomstring @@ many1 @@ atom_string ~arg_ctx:InnerAS
    | _ -> skip
  in
  choice (List.map resolver allow)

(* parses simple_string or brace expansion *)
and sub_arg ~arg_ctx () =
  let { allow } = full_context_by arg_ctx in
  let create seq_begin seq_end incr =
    Fields_of_sequence_expansion.create ~seq_begin ~seq_end ~incr
  in
  let string_be =
    many1 (many1 (simple_string ~arg_ctx ()) <* option " " (string ",") >>| commasep)
    >>= function
    | l when List.length l > 1 -> return @@ stringbraceexp l
    | _ -> fail "String brace expansion must have two or more elements"
  in
  let seq_be p = lift3 create p (string ".." *> p) (option 1 (string ".." *> int)) in
  let literal_exp = seq_be (take_ch ~pred:is_alpha) >>| literalbraceexp in
  let int_exp = seq_be int >>| intbraceexp in
  let brace_expansion =
    braces @@ choice [ string_be; literal_exp; int_exp ] >>| bracedexpansion
  in
  let resolver = function
    | BraceExpansion -> brace_expansion
    | SingleQuotes | DoubleQuotes -> simple_string ~arg_ctx () >>| simplestring
    | _ -> skip
  in
  choice (List.map resolver allow)

(** This is an arg parser.

    For example in "echo hello" "echo" and "hello" are arguments.
    Also for following input is a single arg: echo"hello".
    This is a reason why single_arg has type simple_string list.

    Also arg may be expanded to multiple args.
    For example arg pre{1..3}post will be expanded to three args:
        pre1post pre2post pre3post
    This is a reason why type arg is either SingleArg, either MultipleArgs.

    Also processing should be done intelligently during interpretation,
    but this is done intentionally to simplify the abstraction tree.
    It also allows you to explicitly specify for which types this transformation
    cannot be applied. (see types in ast.ml, which have type single_arg)

    this is in line with the principle that types that are invalid should
    not be representable in a tree
  *)
and arg ~arg_ctx () =
  let collapse_sa s = List.concat s in
  let str_exp = List.map (function CommaSep l -> l) in
  let seq_exp incr seq_begin seq_end to_answer =
    let step = if incr = 0 then 1 else Int.abs incr in
    let sign = Int.compare seq_end seq_begin in
    List.init
      ((Int.abs (seq_end - seq_begin) + 1) / step)
      (fun x -> to_answer @@ (seq_begin + (sign * (step * x))))
  in
  let process_braced_expansion = function
    | StringBraceExp l -> str_exp l
    | LiteralBraceExp seq ->
      Char.(seq_exp seq.incr (code seq.seq_begin) (code seq.seq_end) chr)
      |> List.map (fun x -> [ AtomString [ Text (String.make 1 x) ] ])
    | IntBraceExp seq ->
      seq_exp seq.incr seq.seq_begin seq.seq_end Fun.id
      |> List.map (fun x -> [ AtomString [ Text (Int.to_string x) ] ])
  in
  let reserved_args =
    List.map
      (fun r -> [ AtomString [ Text r ] ])
      [ "do"
      ; "done"
      ; "if"
      ; "for"
      ; "case"
      ; "while"
      ; "until"
      ; "in"
      ; "esac"
      ; "then"
      ; "else"
      ; "elif"
      ; "fi"
      ]
  in
  let process_many x =
    Batteries.List.n_cartesian_product
    @@ List.map
         (function
          | BracedExpansion braced_exp -> process_braced_expansion braced_exp
          | SimpleString ss -> [ [ ss ] ])
         x
  in
  many1 (sub_arg ~arg_ctx ())
  >>| process_many
  >>| (function
        | l when List.length l = 1 -> singlearg (collapse_sa @@ List.hd l)
        | l -> multipleargs (List.map collapse_sa l))
  >>= function
  | SingleArg arg when List.mem arg reserved_args -> fail "Received reserved word"
  | x -> return x

and single_arg ?(arg_ctx = Default) () =
  arg ~arg_ctx ()
  >>= function
  | SingleArg x -> return x
  | _ -> skip

(* parses env variable (atom variable, indexed array or assoc array) *)
and env_var ~arg_ctx () =
  let create key value = Fields_of_key_value.create ~key ~value in
  let single_arg_assgn p =
    lift2 create p (char '=' *> return () >>= single_arg ~arg_ctx)
  in
  let atom_var = lift atomvariable (single_arg_assgn var_p) in
  let array_base item =
    lift2 create name (string "=(" *> many1 (trim item) <* char ')')
  in
  let ind_array = array_base (arg ~arg_ctx ()) >>| indexedarray in
  let assoc_array = array_base (single_arg_assgn name) >>| assocarray in
  choice [ assoc_array; ind_array; atom_var ]

(* parses atom_operand *)
and atom_operand ~arg_ctx () =
  let create invert (rs1, env_vars) (rs2, cmd) =
    Fields_of_atom_operand.create ~invert ~env_vars ~cmd ~redirs:(List.append rs1 rs2)
  in
  lift3
    create
    (option ' ' (char '!' <* char ' ') >>| ( = ) '!')
    (or_parse_many spaces (return () >>= redir_p) (env_var ~arg_ctx () <* spaces))
    (or_parse_many spaces (return () >>= redir_p) (trim (arg ~arg_ctx ())))
  >>= function
  | x when x.cmd = [] && x.env_vars = [] -> fail "Variables or command must be defined"
  | x -> return x

(* parses operand *)
and operand ~arg_ctx () =
  let atom_operand () = lift atomoperand (atom_operand ~arg_ctx ()) in
  let compound () = compound_p () >>| compoundoperand in
  let atom () = return () >>= atom_operand <|> (return () >>= compound) in
  fix (fun operand ->
    lift3
      (fun l sep -> (if sep = "||" then oroperand else andoperand) l)
      (trim_r @@ atom ())
      (trim @@ (string "||" <|> string "&&"))
      (operand <|> (return () >>= atom)))
  <|> (return () >>= atom_operand)

(* parses pipe *)
and pipe ~arg_ctx () =
  lift operands @@ many1 (trim_l @@ operand ~arg_ctx () <* option ' ' (trim (char '|')))

and any_commands () = many1 @@ delim_spaces_around_sc1 (return () >>= any_command)
and group_p () = braces (return () >>= any_commands)

and arithm_compound () =
  parens2 expr_p >>| arithmcompound >>| fun x -> [ compound (x, []) ]

and loop_p () =
  let loop_cmd = sep_by1 delim_spaces_sc (return () >>= any_command) in
  let loop_body = string "do" *> delim_spaces_around_sc1 loop_cmd <* string "done" in
  let for_p () =
    let create head commands = { head; commands } in
    let words = many1 (spaces >>= arg ~arg_ctx:Default) in
    let for_head =
      let words_in = lift2 wordsin (name <* delim_spaces <* string "in") words in
      let triple_inner = lift3 arithmtriple (expr_p <* sc) (expr_p <* sc) expr_p in
      let arithm_triple = parens2 triple_inner in
      string "for" *> spaces *> (words_in <|> arithm_triple) <* spaces_delims_around_sc
    in
    lift2 create for_head loop_body >>| forcompound
  in
  let until_while_p () =
    let create condition cons_cmds = { condition; cons_cmds } in
    map2
      (string "while" <|> string "until")
      (lift2 create (loop_cmd <|> arithm_compound () <* delim_spaces_sc1) loop_body)
      ~f:(fun x -> if x = "while" then whilecompound else untilcompound)
  in
  for_p () <|> until_while_p ()

and if_p () =
  let create condition cons_cmds = { condition; cons_cmds } in
  let commands () = sep_by1 delim_spaces_sc (return () >>= any_command) in
  let lift = lift2 create in
  let if_elif_then key_word =
    let any_commands () = many1 (delim_spaces_sc *> (return () >>= any_command)) in
    lift
      (string key_word *> (return () >>= any_commands) <* delim_spaces_sc1)
      (string "then" *> delim_spaces_sc *> (return () >>= commands) <* delim_spaces_sc)
  in
  (* the condition below is always true *)
  let else_p =
    lift
      (return [ Compound (ArithmCompound (Number 1), []) ])
      (string "else" *> delim_spaces_around_sc1 (return () >>= commands))
  in
  lift3
    (fun l m r -> List.concat [ [ l ]; m; r ])
    (if_elif_then "if")
    (option [] (many (if_elif_then "elif")))
    (option [] (else_p >>| fun x -> [ x ]))
  <* string "fi"

and case_in_p () =
  let arg_ctx = Default in
  let create by cases = { by; cases } in
  let case_start =
    string "case" *> spaces
    >>= single_arg ~arg_ctx
    <* delim_spaces
    <* string "in"
    <* delim_spaces
    <* option "" (string "(")
  in
  let is_first_valid =
    delim_spaces *> peek_char_fail
    >>= fun x ->
    if x = '|' then fail "syntax error near unexpected token '|'" else return ()
  in
  let fail_if_end =
    peek_char_fail >>= fun x -> if x = ')' then fail "end of left part" else return ()
  in
  let element ~is_first =
    let first = is_first_valid <* option "" (string "(") in
    let second = spaces <* fail_if_end <* trim (char '|') in
    (if is_first then first else second) >>= single_arg ~arg_ctx
  in
  let arg =
    lift2
      (fun f i -> f :: i)
      (element ~is_first:true)
      (many (element ~is_first:false) <* char ')')
  in
  let cmd = delim_spaces >>= any_command <* delim_spaces <* sc <* sc in
  lift2 create case_start (many1 (both arg cmd <* delim_spaces) <* string "esac")

and compound_p () =
  choice
    [ group_p () >>| group
    ; loop_p () >>| loop
    ; if_p () >>| ifcompound
    ; case_in_p () >>| casein
    ; lift arithmcompound (option ' ' dollar *> parens2 expr_p)
    ]

and redir_p () =
  let open Utils in
  [ ">>", StdOut, appendoutput
  ; "<&", StdIn, dupinput
  ; ">&", StdOut, dupoutput
  ; "<", StdIn, redirinput
  ; ">", StdOut, rediroutput
  ]
  |> List.map (fun (prefix, fd, create) ->
       lift2 create (option (fd_to_int fd) u_int) (string prefix *> spaces >>= single_arg))
  |> choice

and any_command ?(arg_ctx = Default) () =
  let simple = pipe ~arg_ctx () >>| simple in
  let redirs = many (trim (redir_p ())) in
  let compound = both (compound_p ()) redirs >>| compound in
  simple <|> compound

and parse_function () =
  let create name body = func @@ Fields_of_func.create ~name ~body in
  lift2
    create
    (string "function" *> trim name <* option "" (string "()"))
    (trim @@ braces (return () >>= parse_script))

and parse_script () =
  delim_spaces_sc
  *> many1
       (choice [ return () >>= parse_function; any_command () >>| command ]
       <* delim_spaces_sc)
  <|> return []
;;

let parse str =
  let open Result in
  match parse_string ~consume:Consume.All (parse_script ()) str with
  | Ok x -> Ok x
  | Error er -> Error (`ParsingError er)
;;

type error = [ `ParsingError of string ]

let pp_error ppf = function
  | `ParsingError s -> Format.fprintf ppf "%s" s
;;

(** returns two parsers that can be used for positive and negative tests  *)
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

(* Test int parser *)

let ok_int = test_ok (fun _ -> print_int) int
let fail_int = test_fail (fun _ -> print_int) int

let%test _ = ok_int "10" 10
let%test _ = ok_int "+102890" 102890
let%test _ = ok_int "-21" (-21)
let%test _ = fail_int ""
let%test _ = fail_int "-21-20"
let%test _ = fail_int "+200+"
let%test _ = fail_int "200-"

(** Test variable parser (also tests name parser) *)

let ok_var = test_ok pp_var var_p
let fail_var = test_fail pp_var var_p

let%test _ = ok_var "ka" { name = "ka"; subscript = "0" }
let%test _ = ok_var "_ka" { name = "_ka"; subscript = "0" }
let%test _ = ok_var "less[0]" { name = "less"; subscript = "0" }
let%test _ = ok_var "subs[22]" { name = "subs"; subscript = "22" }
let%test _ = ok_var "s5fb6s[1999]" { name = "s5fb6s"; subscript = "1999" }
let%test _ = ok_var "hm[key]" { name = "hm"; subscript = "key" }
let%test _ = fail_var "   spaces"
let%test _ = fail_var "spaces_after "
let%test _ = fail_var " around_spaces "
let%test _ = fail_var "3789starts_with_digits"
let%test _ = fail_var "sub[]]"

(* Tests text_p parser *)

(* Tests text_p parser inner single quoted string *)

let ok_sqs = test_ok pp_name (text_p ~arg_ctx:InnerSQS)
let fail_sqs = test_fail pp_name (text_p ~arg_ctx:InnerSQS)

let%test _ = ok_sqs "ka" "ka"
let%test _ = ok_sqs {|"some"|} {|"some"|}
let%test _ = ok_sqs {|some"some"|} {|some"some"|}
let%test _ = ok_sqs {|""|} {|""|}

(* In single quotes backslashes are treated as common char *)
let%test _ = ok_sqs {|"so\
me"|} {|"so\
me"|}

let%test _ = ok_sqs {|"so$kadu"|} {|"so$kadu"|}
let%test _ = ok_sqs {|"so$kadu"|} {|"so$kadu"|}
let%test _ = ok_sqs {|"s``o&"|} {|"s``o&"|}
let%test _ = fail_sqs {|"some'some'"|}
let%test _ = fail_sqs {|"'"|}

(* Tests text_p parser inner double quoted string *)

let ok_dqs = test_ok pp_name (text_p ~arg_ctx:InnerDQS)
let fail_dqs = test_fail pp_name (text_p ~arg_ctx:InnerDQS)

let%test _ = ok_dqs "simple" "simple"
let%test _ = ok_dqs "ka\\kadu" "ka\\kadu"
let%test _ = ok_dqs "ka\\\\kadu" "ka\\kadu"
let%test _ = ok_dqs "ka\\$kadu" "ka$kadu"
let%test _ = ok_dqs "ka\\\nkadu" "ka\nkadu"
let%test _ = ok_dqs "some\\f" "some\\f"
let%test _ = ok_dqs "hm\\\"" "hm\""
let%test _ = ok_dqs "h\\\"m\\\"" "h\"m\""
let%test _ = fail_dqs {|so$me|}
let%test _ = fail_dqs {|some"|}
let%test _ = fail_dqs {|"some"|}
let%test _ = fail_dqs {|$so"me|}

(* Tests text_p parser inner atom string *)

let ok_ias = test_ok pp_name (text_p ~arg_ctx:InnerAS)
let fail_ias = test_fail pp_name (text_p ~arg_ctx:InnerAS)

let%test _ = ok_ias "simple" "simple"
let%test _ = ok_ias "12some12" "12some12"
let%test _ = ok_ias "defaul#12*tstr" "defaul#12*tstr"

let%test _ = ok_ias {|some\
so|} {|someso|}

let%test _ = ok_ias {|so\
me\
so|} {|someso|}

let%test _ = ok_ias {|some\n\m\l|} {|somenml|}
let%test _ = fail_ias {|so$me|}
let%test _ = fail_ias {|some"|}
let%test _ = fail_ias {|some'|}
let%test _ = fail_ias {|$so"me|}
let%test _ = fail_ias {||}
let%test _ = fail_ias {|some(what)|}
let%test _ = fail_ias {|{what}|}

(* Arithmetic expressions parser tests *)

let ok_expr = test_ok pp_expr expr_p
let fail_expr = test_fail pp_expr expr_p

let%test _ = ok_expr {|1|} (Number 1)
let%test _ = ok_expr {|1+2|} (Plus (Number 1, Number 2))
let%test _ = ok_expr {|1-2|} (Minus (Number 1, Number 2))
let%test _ = ok_expr {|1*2|} (Asterisk (Number 1, Number 2))
let%test _ = ok_expr {|1/2|} (Slash (Number 1, Number 2))
let%test _ = ok_expr {|1>2|} (Greater (Number 1, Number 2))
let%test _ = ok_expr {|1>=2|} (GreaterOrEqual (Number 1, Number 2))
let%test _ = ok_expr {|1<2|} (Less (Number 1, Number 2))
let%test _ = ok_expr {|1<=2|} (LessOrEqual (Number 1, Number 2))
let%test _ = ok_expr {|1=2|} (Equal (Number 1, Number 2))
let%test _ = ok_expr {|1!=2|} (NotEqual (Number 1, Number 2))
let%test _ = ok_expr {|1<(2)|} (Less (Number 1, Number 2))
let%test _ = ok_expr {|(1!=2)|} (NotEqual (Number 1, Number 2))
let%test _ = ok_expr {|(((1))!=(2))|} (NotEqual (Number 1, Number 2))

let%test _ =
  ok_expr
    {|(1+(2*8))>4|}
    (Greater (Plus (Number 1, Asterisk (Number 2, Number 8)), Number 4))
;;

let%test _ =
  ok_expr
    {|(1+2*8)>4|}
    (Greater (Plus (Number 1, Asterisk (Number 2, Number 8)), Number 4))
;;

let%test _ =
  ok_expr {|1+2*8>4|} (Greater (Plus (Number 1, Asterisk (Number 2, Number 8)), Number 4))
;;

let%test _ =
  ok_expr
    {|(1+2)*8>4|}
    (Greater (Asterisk (Plus (Number 1, Number 2), Number 8), Number 4))
;;

let%test _ = fail_expr {|(1+2)*8>4)|}
let%test _ = fail_expr {|(((1+2)*8>4)|}
let%test _ = fail_expr {|(((1+(2)*8>4)|}
let%test _ = fail_expr {||}
let%test _ = fail_expr {|-|}
let%test _ = fail_expr {|+|}
let%test _ = fail_expr {|8*|}

(** Param expansion parsers tests *)

let ok_pe = test_ok pp_atom_string param_exp
let fail_pe = test_fail pp_atom_string param_exp

(** VarNameExpansion parser *)

let%test _ = ok_pe {|$so|} (ParamExpansion (VarNameExpansion "so"))
let%test _ = ok_pe {|$ka12|} (ParamExpansion (VarNameExpansion "ka12"))
let%test _ = ok_pe {|$so|} (ParamExpansion (VarNameExpansion "so"))
let%test _ = fail_pe {|$$so|}
let%test _ = fail_pe {|$1o|}
let%test _ = fail_pe {|$so$|}
let%test _ = fail_pe {|$  so|}
let%test _ = fail_pe {|  $so|}
let%test _ = fail_pe {|$so  |}

(** Var expansion parser *)

let%test _ =
  ok_pe {|${so}|} (ParamExpansion (VarExpansion { name = "so"; subscript = "0" }))
;;

let%test _ =
  ok_pe {|${ka12}|} (ParamExpansion (VarExpansion { name = "ka12"; subscript = "0" }))
;;

let%test _ =
  ok_pe {|${so}|} (ParamExpansion (VarExpansion { name = "so"; subscript = "0" }))
;;

let%test _ = fail_pe {|$${so}|}
let%test _ = fail_pe {|${1o}|}
let%test _ = fail_pe {|${so}$|}
let%test _ = fail_pe {|  ${so}|}

(** PositionalParam parser *)

let%test _ = ok_pe {|$1|} (ParamExpansion (PositionalParam 1))
let%test _ = ok_pe {|$9|} (ParamExpansion (PositionalParam 9))
let%test _ = ok_pe {|$5|} (ParamExpansion (PositionalParam 5))
let%test _ = ok_pe {|$0|} (ParamExpansion (PositionalParam 0))
let%test _ = ok_pe {|${0}|} (ParamExpansion (PositionalParam 0))
let%test _ = ok_pe {|${9}|} (ParamExpansion (PositionalParam 9))
let%test _ = ok_pe {|${20}|} (ParamExpansion (PositionalParam 20))
let%test _ = ok_pe {|${999}|} (ParamExpansion (PositionalParam 999))
let%test _ = fail_pe {|$12|}
let%test _ = fail_pe {|$1 |}
let%test _ = fail_pe {|$1hjkl|}
let%test _ = fail_pe {|$|}
let%test _ = fail_pe {|$990|}
let%test _ = fail_pe {| $990|}
let%test _ = fail_pe {| ${990}|}
let%test _ = fail_pe {| ${1}|}
let%test _ = fail_pe {| ${1hjk}|}

(** Length parser *)

let%test _ =
  ok_pe {|${#name}|} (ParamExpansion (Length { name = "name"; subscript = "0" }))
;;

let%test _ =
  ok_pe {|${#name[0]}|} (ParamExpansion (Length { name = "name"; subscript = "0" }))
;;

let%test _ =
  ok_pe {|${#name[39]}|} (ParamExpansion (Length { name = "name"; subscript = "39" }))
;;

let%test _ = fail_pe {|${##so}|}
let%test _ = fail_pe {|${#1so}|}
let%test _ = fail_pe {|${#so[1}|}
let%test _ = fail_pe {| ${#len}|}

(** Substring parser *)

let%test _ =
  ok_pe
    {|${param:0:1}|}
    (ParamExpansion
       (Substring
          { name = { name = "param"; subscript = "0" }
          ; offset = Number 0
          ; length = Some (Number 1)
          }))
;;

let%test _ =
  ok_pe
    {|${param:227}|}
    (ParamExpansion
       (Substring
          { name = { name = "param"; subscript = "0" }
          ; offset = Number 227
          ; length = None
          }))
;;

let%test _ =
  ok_pe
    {|${param[3]:227}|}
    (ParamExpansion
       (Substring
          { name = { name = "param"; subscript = "3" }
          ; offset = Number 227
          ; length = None
          }))
;;

let%test _ = fail_pe {| ${param:227}|}
let%test _ = fail_pe {|${param::99}|}
let%test _ = fail_pe {|${param::}|}
let%test _ = fail_pe {|${param:}|}

(** Substring removal parser *)

let%test _ =
  ok_pe
    {|${name#pattern}|}
    (ParamExpansion
       (SubstrRemoval
          { name = { name = "name"; subscript = "0" }
          ; pattern = "pattern"
          ; from = FromBegin
          ; min_or_max = Min
          }))
;;

let%test _ =
  ok_pe
    {|${name##pattern}|}
    (ParamExpansion
       (SubstrRemoval
          { name = { name = "name"; subscript = "0" }
          ; pattern = "pattern"
          ; from = FromBegin
          ; min_or_max = Max
          }))
;;

let%test _ =
  ok_pe
    {|${name%pattern}|}
    (ParamExpansion
       (SubstrRemoval
          { name = { name = "name"; subscript = "0" }
          ; pattern = "pattern"
          ; from = FromEnd
          ; min_or_max = Min
          }))
;;

let%test _ =
  ok_pe
    {|${name%%pattern}|}
    (ParamExpansion
       (SubstrRemoval
          { name = { name = "name"; subscript = "0" }
          ; pattern = "pattern"
          ; from = FromEnd
          ; min_or_max = Max
          }))
;;

let%test _ =
  ok_pe
    {|${name[1]%%pattern}|}
    (ParamExpansion
       (SubstrRemoval
          { name = { name = "name"; subscript = "1" }
          ; pattern = "pattern"
          ; from = FromEnd
          ; min_or_max = Max
          }))
;;

let%test _ =
  ok_pe
    {|${name[key]%%pattern}|}
    (ParamExpansion
       (SubstrRemoval
          { name = { name = "name"; subscript = "key" }
          ; pattern = "pattern"
          ; from = FromEnd
          ; min_or_max = Max
          }))
;;

let%test _ =
  ok_pe
    {|${name%#pattern}|}
    (ParamExpansion
       (SubstrRemoval
          { name = { name = "name"; subscript = "0" }
          ; pattern = "#pattern"
          ; from = FromEnd
          ; min_or_max = Min
          }))
;;

let%test _ = fail_pe {|${name%}|}
let%test _ = fail_pe {|${name##}|}
let%test _ = fail_pe {|${%pattern}|}
let%test _ = fail_pe {|${[0]##so}|}
let%test _ = fail_pe {|${[0]##so|}

(** Substitute parser *)

let%test _ =
  ok_pe
    {|${name/pattern}|}
    (ParamExpansion
       (Substitute
          { name = { name = "name"; subscript = "0" }
          ; pattern = "pattern"
          ; by = ""
          ; subst_type = One
          }))
;;

let%test _ =
  ok_pe
    {|${name//pattern}|}
    (ParamExpansion
       (Substitute
          { name = { name = "name"; subscript = "0" }
          ; pattern = "pattern"
          ; by = ""
          ; subst_type = All
          }))
;;

let%test _ =
  ok_pe
    {|${name/#pattern}|}
    (ParamExpansion
       (Substitute
          { name = { name = "name"; subscript = "0" }
          ; pattern = "pattern"
          ; by = ""
          ; subst_type = First
          }))
;;

let%test _ =
  ok_pe
    {|${name/%pattern}|}
    (ParamExpansion
       (Substitute
          { name = { name = "name"; subscript = "0" }
          ; pattern = "pattern"
          ; by = ""
          ; subst_type = Last
          }))
;;

let%test _ =
  ok_pe
    {|${name/#pattern/str}|}
    (ParamExpansion
       (Substitute
          { name = { name = "name"; subscript = "0" }
          ; pattern = "pattern"
          ; by = "str"
          ; subst_type = First
          }))
;;

let%test _ = fail_pe {|${/$pa/s}|}
let%test _ = fail_pe {|$ {/$pa/s}|}
let%test _ = fail_pe {|${//pa}|}

(* Simple string and brace expansion parser tests *)

let ok_sub_arg =
  test_ok pp_braced_expansion_or_simple_string (sub_arg ~arg_ctx:Default ())
;;

let fail_sub_arg =
  test_fail pp_braced_expansion_or_simple_string (sub_arg ~arg_ctx:Default ())
;;

let%test _ = ok_sub_arg {|simple|} (SimpleString (AtomString [ Text "simple" ]))
let%test _ = ok_sub_arg {|"twoone"|} (SimpleString (DoubleQuotedString [ Text "twoone" ]))
let%test _ = ok_sub_arg {|'some'|} (SimpleString (SingleQuotedString "some"))
let%test _ = fail_sub_arg {|'some'"|}
let%test _ = fail_sub_arg {|'some'gm"|}

let%test _ =
  ok_sub_arg
    {|{a,b,c}|}
    (BracedExpansion
       (StringBraceExp
          [ CommaSep [ AtomString [ Text "a" ] ]
          ; CommaSep [ AtomString [ Text "b" ] ]
          ; CommaSep [ AtomString [ Text "c" ] ]
          ]))
;;

let%test _ =
  ok_sub_arg
    {|{some,some,"some"some'some'}|}
    (BracedExpansion
       (StringBraceExp
          [ CommaSep [ AtomString [ Text "some" ] ]
          ; CommaSep [ AtomString [ Text "some" ] ]
          ; CommaSep
              [ DoubleQuotedString [ Text "some" ]
              ; AtomString [ Text "some" ]
              ; SingleQuotedString "some"
              ]
          ]))
;;

let%test _ =
  ok_sub_arg
    {|{$some,$((1)),"$some"$some'some'}|}
    (BracedExpansion
       (StringBraceExp
          [ CommaSep [ AtomString [ ParamExpansion (VarNameExpansion "some") ] ]
          ; CommaSep [ AtomString [ ArithmExpansion (Number 1) ] ]
          ; CommaSep
              [ DoubleQuotedString [ ParamExpansion (VarNameExpansion "some") ]
              ; AtomString [ ParamExpansion (VarNameExpansion "some") ]
              ; SingleQuotedString "some"
              ]
          ]))
;;

let%test _ = fail_sub_arg {|{'some'gm"}|}
let%test _ = fail_sub_arg {|{a,}|}
let%test _ = fail_sub_arg {|{abc,${abc,bde}}|}
let%test _ = fail_sub_arg {|{,vde}|}

let%test _ =
  ok_sub_arg
    {|{d..a..0}|}
    (BracedExpansion (LiteralBraceExp { seq_begin = 'd'; seq_end = 'a'; incr = 0 }))
;;

let%test _ =
  ok_sub_arg
    {|{d..a}|}
    (BracedExpansion (LiteralBraceExp { seq_begin = 'd'; seq_end = 'a'; incr = 1 }))
;;

let%test _ = fail_sub_arg {|{dd..a}|}
let%test _ = fail_sub_arg {|{d..kka}|}
let%test _ = fail_sub_arg {|{d..a..3d}|}
let%test _ = fail_sub_arg {|{d..a..d}|}

let%test _ =
  ok_sub_arg
    {|{1..-5..0}|}
    (BracedExpansion (IntBraceExp { seq_begin = 1; seq_end = -5; incr = 0 }))
;;

let%test _ =
  ok_sub_arg
    {|{1..-5}|}
    (BracedExpansion (IntBraceExp { seq_begin = 1; seq_end = -5; incr = 1 }))
;;

let%test _ =
  ok_sub_arg
    {|{199..80}|}
    (BracedExpansion (IntBraceExp { seq_begin = 199; seq_end = 80; incr = 1 }))
;;

let%test _ =
  ok_sub_arg
    {|{1..-5..-200}|}
    (BracedExpansion (IntBraceExp { seq_begin = 1; seq_end = -5; incr = -200 }))
;;

let%test _ = fail_sub_arg {|{1..4..a}|}
let%test _ = fail_sub_arg {|{1.4..2}|}
let%test _ = fail_sub_arg {|{1.4..2}|}
let%test _ = fail_sub_arg {|{1..4.2}|}
let%test _ = fail_sub_arg {|{1..4.}|}

(* Argument parser tests *)

let ok_arg = test_ok pp_arg (arg ~arg_ctx:Default ())
let fail_arg = test_fail pp_arg (arg ~arg_ctx:Default ())

let%test _ =
  ok_arg
    {|{b,d}d|}
    (MultipleArgs
       [ [ AtomString [ Text "b" ]; AtomString [ Text "d" ] ]
       ; [ AtomString [ Text "d" ]; AtomString [ Text "d" ] ]
       ])
;;

let%test _ =
  ok_arg
    {|d{b,d}d|}
    (MultipleArgs
       [ [ AtomString [ Text "d" ]; AtomString [ Text "b" ]; AtomString [ Text "d" ] ]
       ; [ AtomString [ Text "d" ]; AtomString [ Text "d" ]; AtomString [ Text "d" ] ]
       ])
;;

let%test _ =
  ok_arg
    {|p{a,b}|}
    (MultipleArgs
       [ [ AtomString [ Text "p" ]; AtomString [ Text "a" ] ]
       ; [ AtomString [ Text "p" ]; AtomString [ Text "b" ] ]
       ])
;;

let%test _ =
  ok_arg
    {|"some"'some'{b,d}d|}
    (MultipleArgs
       [ [ DoubleQuotedString [ Text "some" ]
         ; SingleQuotedString "some"
         ; AtomString [ Text "b" ]
         ; AtomString [ Text "d" ]
         ]
       ; [ DoubleQuotedString [ Text "some" ]
         ; SingleQuotedString "some"
         ; AtomString [ Text "d" ]
         ; AtomString [ Text "d" ]
         ]
       ])
;;

let%test _ =
  ok_arg
    {|from{a,b}inn"some"{c,d}postsomething|}
    (MultipleArgs
       [ [ AtomString [ Text "from" ]
         ; AtomString [ Text "a" ]
         ; AtomString [ Text "inn" ]
         ; DoubleQuotedString [ Text "some" ]
         ; AtomString [ Text "c" ]
         ; AtomString [ Text "postsomething" ]
         ]
       ; [ AtomString [ Text "from" ]
         ; AtomString [ Text "a" ]
         ; AtomString [ Text "inn" ]
         ; DoubleQuotedString [ Text "some" ]
         ; AtomString [ Text "d" ]
         ; AtomString [ Text "postsomething" ]
         ]
       ; [ AtomString [ Text "from" ]
         ; AtomString [ Text "b" ]
         ; AtomString [ Text "inn" ]
         ; DoubleQuotedString [ Text "some" ]
         ; AtomString [ Text "c" ]
         ; AtomString [ Text "postsomething" ]
         ]
       ; [ AtomString [ Text "from" ]
         ; AtomString [ Text "b" ]
         ; AtomString [ Text "inn" ]
         ; DoubleQuotedString [ Text "some" ]
         ; AtomString [ Text "d" ]
         ; AtomString [ Text "postsomething" ]
         ]
       ])
;;

let%test _ = fail_arg {|"some'some'{b,d}d|}
let%test _ = fail_arg {|some'some'{b}d|}
let%test _ = fail_arg {|"some'some{b,d}d|}

(* Environment variable assignment parser tests *)

let ok_env_var = test_ok pp_var_assign (env_var ~arg_ctx:Default ())
let fail_env_var = test_fail pp_var_assign (env_var ~arg_ctx:Default ())

let%test _ =
  ok_env_var
    {|k=${name/pattern/string}|}
    (AtomVariable
       { key = { name = "k"; subscript = "0" }
       ; value =
           [ AtomString
               [ ParamExpansion
                   (Substitute
                      { name = { name = "name"; subscript = "0" }
                      ; pattern = "pattern"
                      ; by = "string"
                      ; subst_type = One
                      })
               ]
           ]
       })
;;

let%test _ =
  ok_env_var
    {|k=${name//pattern}|}
    (AtomVariable
       { key = { name = "k"; subscript = "0" }
       ; value =
           [ AtomString
               [ ParamExpansion
                   (Substitute
                      { name = { name = "name"; subscript = "0" }
                      ; pattern = "pattern"
                      ; by = ""
                      ; subst_type = All
                      })
               ]
           ]
       })
;;

let%test _ =
  ok_env_var
    {|k=${name/#pattern}|}
    (AtomVariable
       { key = { name = "k"; subscript = "0" }
       ; value =
           [ AtomString
               [ ParamExpansion
                   (Substitute
                      { name = { name = "name"; subscript = "0" }
                      ; pattern = "pattern"
                      ; by = ""
                      ; subst_type = First
                      })
               ]
           ]
       })
;;

let%test _ =
  ok_env_var
    {|k=${name2/%pattern}|}
    (AtomVariable
       { key = { name = "k"; subscript = "0" }
       ; value =
           [ AtomString
               [ ParamExpansion
                   (Substitute
                      { name = { name = "name2"; subscript = "0" }
                      ; pattern = "pattern"
                      ; by = ""
                      ; subst_type = Last
                      })
               ]
           ]
       })
;;

let%test _ =
  ok_env_var
    {|k=${#k}|}
    (AtomVariable
       { key = { name = "k"; subscript = "0" }
       ; value =
           [ AtomString [ ParamExpansion (Length { name = "k"; subscript = "0" }) ] ]
       })
;;

let%test _ =
  ok_env_var
    {|k=${name:a=10+2:b=c+d}|}
    (AtomVariable
       { key = { name = "k"; subscript = "0" }
       ; value =
           [ AtomString
               [ ParamExpansion
                   (Substring
                      { name = { name = "name"; subscript = "0" }
                      ; offset =
                          Assignment
                            ({ name = "a"; subscript = "0" }, Plus (Number 10, Number 2))
                      ; length =
                          Some
                            (Assignment
                               ( { name = "b"; subscript = "0" }
                               , Plus
                                   ( Variable { name = "c"; subscript = "0" }
                                   , Variable { name = "d"; subscript = "0" } ) ))
                      })
               ]
           ]
       })
;;

let%test _ =
  ok_env_var
    {|k=${MYSTRING##*in}|}
    (AtomVariable
       { key = { name = "k"; subscript = "0" }
       ; value =
           [ AtomString
               [ ParamExpansion
                   (SubstrRemoval
                      { name = { name = "MYSTRING"; subscript = "0" }
                      ; pattern = "*in"
                      ; from = FromBegin
                      ; min_or_max = Max
                      })
               ]
           ]
       })
;;

let%test _ =
  ok_env_var
    {|k=${MYSTRING%*in}|}
    (AtomVariable
       { key = { name = "k"; subscript = "0" }
       ; value =
           [ AtomString
               [ ParamExpansion
                   (SubstrRemoval
                      { name = { name = "MYSTRING"; subscript = "0" }
                      ; pattern = "*in"
                      ; from = FromEnd
                      ; min_or_max = Min
                      })
               ]
           ]
       })
;;

let%test _ =
  ok_env_var
    {|k=${MYSTRING%%*in}|}
    (AtomVariable
       { key = { name = "k"; subscript = "0" }
       ; value =
           [ AtomString
               [ ParamExpansion
                   (SubstrRemoval
                      { name = { name = "MYSTRING"; subscript = "0" }
                      ; pattern = "*in"
                      ; from = FromEnd
                      ; min_or_max = Max
                      })
               ]
           ]
       })
;;

let%test _ =
  ok_env_var
    {|k=ke\ek|}
    (AtomVariable
       { key = { name = "k"; subscript = "0" }; value = [ AtomString [ Text "keek" ] ] })
;;

let%test _ =
  ok_env_var
    {|a[23]=10|}
    (AtomVariable
       { key = { name = "a"; subscript = "23" }; value = [ AtomString [ Text "10" ] ] })
;;

let%test _ =
  ok_env_var
    {|a=(k=10 b=10)|}
    (AssocArray
       { key = "a"
       ; value =
           [ { key = "k"; value = [ AtomString [ Text "10" ] ] }
           ; { key = "b"; value = [ AtomString [ Text "10" ] ] }
           ]
       })
;;

let%test _ =
  ok_env_var
    {|a=$8+9|}
    (AtomVariable
       { key = { name = "a"; subscript = "0" }
       ; value = [ AtomString [ ParamExpansion (PositionalParam 8); Text "+9" ] ]
       })
;;

(* Yes, this test is correct *)
let%test _ =
  ok_env_var
    {|a=$87+9|}
    (AtomVariable
       { key = { name = "a"; subscript = "0" }
       ; value = [ AtomString [ ParamExpansion (PositionalParam 8); Text "7+9" ] ]
       })
;;

let%test _ =
  ok_env_var
    {|a=$((8+9))|}
    (AtomVariable
       { key = { name = "a"; subscript = "0" }
       ; value = [ AtomString [ ArithmExpansion (Plus (Number 8, Number 9)) ] ]
       })
;;

let%test _ =
  ok_env_var
    {|k=( 3  6)|}
    (IndexedArray
       { key = "k"
       ; value =
           [ SingleArg [ AtomString [ Text "3" ] ]
           ; SingleArg [ AtomString [ Text "6" ] ]
           ]
       })
;;

(* This test is correct *)
let%test _ =
  ok_env_var
    {|echo=(  smth  =  ke   )|}
    (IndexedArray
       { key = "echo"
       ; value =
           [ SingleArg [ AtomString [ Text "smth" ] ]
           ; SingleArg [ AtomString [ Text "=" ] ]
           ; SingleArg [ AtomString [ Text "ke" ] ]
           ]
       })
;;

let%test _ =
  ok_env_var
    {|echo=(smth ki=ke2="3=228" ke3)|}
    (IndexedArray
       { key = "echo"
       ; value =
           [ SingleArg [ AtomString [ Text "smth" ] ]
           ; SingleArg
               [ AtomString [ Text "ki=ke2=" ]; DoubleQuotedString [ Text "3=228" ] ]
           ; SingleArg [ AtomString [ Text "ke3" ] ]
           ]
       })
;;

let%test _ =
  ok_env_var
    {|echo=(ke="ke2=""3=228")|}
    (AssocArray
       { key = "echo"
       ; value =
           [ { key = "ke"
             ; value =
                 [ DoubleQuotedString [ Text "ke2=" ]
                 ; DoubleQuotedString [ Text "3=228" ]
                 ]
             }
           ]
       })
;;

let%test _ = fail_env_var {|echo  =  smth|}
let%test _ = fail_env_var {|a=d${a}{b,d}"c"|}

(* Pipe parser tests *)

let ok_pipe = test_ok pp_pipe (pipe ~arg_ctx:Default ())
let fail_pipe = test_fail pp_pipe (pipe ~arg_ctx:Default ())

let%test _ =
  ok_pipe
    {|echo || pritn|}
    (Operands
       [ OrOperand
           ( AtomOperand
               { invert = false
               ; env_vars = []
               ; cmd = [ SingleArg [ AtomString [ Text "echo" ] ] ]
               ; redirs = []
               }
           , AtomOperand
               { invert = false
               ; env_vars = []
               ; cmd = [ SingleArg [ AtomString [ Text "pritn" ] ] ]
               ; redirs = []
               } )
       ])
;;

let%test _ =
  ok_pipe
    {|echo | pritn || some && some|}
    (Operands
       [ AtomOperand
           { invert = false
           ; env_vars = []
           ; cmd = [ SingleArg [ AtomString [ Text "echo" ] ] ]
           ; redirs = []
           }
       ; OrOperand
           ( AtomOperand
               { invert = false
               ; env_vars = []
               ; cmd = [ SingleArg [ AtomString [ Text "pritn" ] ] ]
               ; redirs = []
               }
           , AndOperand
               ( AtomOperand
                   { invert = false
                   ; env_vars = []
                   ; cmd = [ SingleArg [ AtomString [ Text "some" ] ] ]
                   ; redirs = []
                   }
               , AtomOperand
                   { invert = false
                   ; env_vars = []
                   ; cmd = [ SingleArg [ AtomString [ Text "some" ] ] ]
                   ; redirs = []
                   } ) )
       ])
;;

let%test _ =
  ok_pipe
    {|echo hello || print something|}
    (Operands
       [ OrOperand
           ( AtomOperand
               { invert = false
               ; env_vars = []
               ; cmd =
                   [ SingleArg [ AtomString [ Text "echo" ] ]
                   ; SingleArg [ AtomString [ Text "hello" ] ]
                   ]
               ; redirs = []
               }
           , AtomOperand
               { invert = false
               ; env_vars = []
               ; cmd =
                   [ SingleArg [ AtomString [ Text "print" ] ]
                   ; SingleArg [ AtomString [ Text "something" ] ]
                   ]
               ; redirs = []
               } )
       ])
;;

let%test _ =
  ok_pipe
    "! echo hello | some | wt 'sommand' 'sos eom '"
    (Operands
       [ AtomOperand
           { invert = true
           ; env_vars = []
           ; cmd =
               [ SingleArg [ AtomString [ Text "echo" ] ]
               ; SingleArg [ AtomString [ Text "hello" ] ]
               ]
           ; redirs = []
           }
       ; AtomOperand
           { invert = false
           ; env_vars = []
           ; cmd = [ SingleArg [ AtomString [ Text "some" ] ] ]
           ; redirs = []
           }
       ; AtomOperand
           { invert = false
           ; env_vars = []
           ; cmd =
               [ SingleArg [ AtomString [ Text "wt" ] ]
               ; SingleArg [ SingleQuotedString "sommand" ]
               ; SingleArg [ SingleQuotedString "sos eom " ]
               ]
           ; redirs = []
           }
       ])
;;

let%test _ =
  ok_pipe
    "echo=some'some' echo 'ro'th'something'"
    (Operands
       [ AtomOperand
           { invert = false
           ; env_vars =
               [ AtomVariable
                   { key = { name = "echo"; subscript = "0" }
                   ; value = [ AtomString [ Text "some" ]; SingleQuotedString "some" ]
                   }
               ]
           ; cmd =
               [ SingleArg [ AtomString [ Text "echo" ] ]
               ; SingleArg
                   [ SingleQuotedString "ro"
                   ; AtomString [ Text "th" ]
                   ; SingleQuotedString "something"
                   ]
               ]
           ; redirs = []
           }
       ])
;;

let%test _ =
  ok_pipe
    {|print something || echo hello || em lo && lo || ke | wow && ke |}
    (Operands
       [ OrOperand
           ( AtomOperand
               { invert = false
               ; env_vars = []
               ; cmd =
                   [ SingleArg [ AtomString [ Text "print" ] ]
                   ; SingleArg [ AtomString [ Text "something" ] ]
                   ]
               ; redirs = []
               }
           , OrOperand
               ( AtomOperand
                   { invert = false
                   ; env_vars = []
                   ; cmd =
                       [ SingleArg [ AtomString [ Text "echo" ] ]
                       ; SingleArg [ AtomString [ Text "hello" ] ]
                       ]
                   ; redirs = []
                   }
               , AndOperand
                   ( AtomOperand
                       { invert = false
                       ; env_vars = []
                       ; cmd =
                           [ SingleArg [ AtomString [ Text "em" ] ]
                           ; SingleArg [ AtomString [ Text "lo" ] ]
                           ]
                       ; redirs = []
                       }
                   , OrOperand
                       ( AtomOperand
                           { invert = false
                           ; env_vars = []
                           ; cmd = [ SingleArg [ AtomString [ Text "lo" ] ] ]
                           ; redirs = []
                           }
                       , AtomOperand
                           { invert = false
                           ; env_vars = []
                           ; cmd = [ SingleArg [ AtomString [ Text "ke" ] ] ]
                           ; redirs = []
                           } ) ) ) )
       ; AndOperand
           ( AtomOperand
               { invert = false
               ; env_vars = []
               ; cmd = [ SingleArg [ AtomString [ Text "wow" ] ] ]
               ; redirs = []
               }
           , AtomOperand
               { invert = false
               ; env_vars = []
               ; cmd = [ SingleArg [ AtomString [ Text "ke" ] ] ]
               ; redirs = []
               } )
       ])
;;

let%test _ =
  ok_pipe
    {|! echo |}
    (Operands
       [ AtomOperand
           { invert = true
           ; env_vars = []
           ; cmd = [ SingleArg [ AtomString [ Text "echo" ] ] ]
           ; redirs = []
           }
       ])
;;

let%test _ =
  ok_pipe
    {|! var=1 echo|}
    (Operands
       [ AtomOperand
           { invert = true
           ; env_vars =
               [ AtomVariable
                   { key = { name = "var"; subscript = "0" }
                   ; value = [ AtomString [ Text "1" ] ]
                   }
               ]
           ; cmd = [ SingleArg [ AtomString [ Text "echo" ] ] ]
           ; redirs = []
           }
       ])
;;

let%test _ =
  ok_pipe
    {|! var=1 var2=2 echo |}
    (Operands
       [ AtomOperand
           { invert = true
           ; env_vars =
               [ AtomVariable
                   { key = { name = "var"; subscript = "0" }
                   ; value = [ AtomString [ Text "1" ] ]
                   }
               ; AtomVariable
                   { key = { name = "var2"; subscript = "0" }
                   ; value = [ AtomString [ Text "2" ] ]
                   }
               ]
           ; cmd = [ SingleArg [ AtomString [ Text "echo" ] ] ]
           ; redirs = []
           }
       ])
;;

let%test _ =
  ok_pipe
    {|`! var=1 echo`|}
    (Operands
       [ AtomOperand
           { invert = false
           ; env_vars = []
           ; cmd =
               [ SingleArg
                   [ AtomString
                       [ CommandSubstitution
                           (Operands
                              [ AtomOperand
                                  { invert = true
                                  ; env_vars =
                                      [ AtomVariable
                                          { key = { name = "var"; subscript = "0" }
                                          ; value = [ AtomString [ Text "1" ] ]
                                          }
                                      ]
                                  ; cmd = [ SingleArg [ AtomString [ Text "echo" ] ] ]
                                  ; redirs = []
                                  }
                              ])
                       ]
                   ]
               ]
           ; redirs = []
           }
       ])
;;

let%test _ =
  ok_pipe
    {|`! var=1 >some.txt echo`|}
    (Operands
       [ AtomOperand
           { invert = false
           ; env_vars = []
           ; cmd =
               [ SingleArg
                   [ AtomString
                       [ CommandSubstitution
                           (Operands
                              [ AtomOperand
                                  { invert = true
                                  ; env_vars =
                                      [ AtomVariable
                                          { key = { name = "var"; subscript = "0" }
                                          ; value = [ AtomString [ Text "1" ] ]
                                          }
                                      ]
                                  ; cmd = [ SingleArg [ AtomString [ Text "echo" ] ] ]
                                  ; redirs =
                                      [ RedirOutput (1, [ AtomString [ Text "some.txt" ] ])
                                      ]
                                  }
                              ])
                       ]
                   ]
               ]
           ; redirs = []
           }
       ])
;;

let%test _ =
  ok_pipe
    {|`! var=1 >some.txt echo <from.txt`|}
    (Operands
       [ AtomOperand
           { invert = false
           ; env_vars = []
           ; cmd =
               [ SingleArg
                   [ AtomString
                       [ CommandSubstitution
                           (Operands
                              [ AtomOperand
                                  { invert = true
                                  ; env_vars =
                                      [ AtomVariable
                                          { key = { name = "var"; subscript = "0" }
                                          ; value = [ AtomString [ Text "1" ] ]
                                          }
                                      ]
                                  ; cmd = [ SingleArg [ AtomString [ Text "echo" ] ] ]
                                  ; redirs =
                                      [ RedirOutput (1, [ AtomString [ Text "some.txt" ] ])
                                      ; RedirInput (0, [ AtomString [ Text "from.txt" ] ])
                                      ]
                                  }
                              ])
                       ]
                   ]
               ]
           ; redirs = []
           }
       ])
;;

let%test _ =
  ok_pipe
    {|`>some.txt echo <from.txt hello`|}
    (Operands
       [ AtomOperand
           { invert = false
           ; env_vars = []
           ; cmd =
               [ SingleArg
                   [ AtomString
                       [ CommandSubstitution
                           (Operands
                              [ AtomOperand
                                  { invert = false
                                  ; env_vars = []
                                  ; cmd =
                                      [ SingleArg [ AtomString [ Text "echo" ] ]
                                      ; SingleArg [ AtomString [ Text "hello" ] ]
                                      ]
                                  ; redirs =
                                      [ RedirOutput (1, [ AtomString [ Text "some.txt" ] ])
                                      ; RedirInput (0, [ AtomString [ Text "from.txt" ] ])
                                      ]
                                  }
                              ])
                       ]
                   ]
               ]
           ; redirs = []
           }
       ])
;;

let%test _ =
  ok_pipe
    {|kek=10 some=5 printenv | wc|}
    (Operands
       [ AtomOperand
           { invert = false
           ; env_vars =
               [ AtomVariable
                   { key = { name = "kek"; subscript = "0" }
                   ; value = [ AtomString [ Text "10" ] ]
                   }
               ; AtomVariable
                   { key = { name = "some"; subscript = "0" }
                   ; value = [ AtomString [ Text "5" ] ]
                   }
               ]
           ; cmd = [ SingleArg [ AtomString [ Text "printenv" ] ] ]
           ; redirs = []
           }
       ; AtomOperand
           { invert = false
           ; env_vars = []
           ; cmd = [ SingleArg [ AtomString [ Text "wc" ] ] ]
           ; redirs = []
           }
       ])
;;

let%test _ =
  ok_pipe
    {|>some.txt echo <from.txt hello | ok let\'s go <&from.txt|}
    (Operands
       [ AtomOperand
           { invert = false
           ; env_vars = []
           ; cmd =
               [ SingleArg [ AtomString [ Text "echo" ] ]
               ; SingleArg [ AtomString [ Text "hello" ] ]
               ]
           ; redirs =
               [ RedirOutput (1, [ AtomString [ Text "some.txt" ] ])
               ; RedirInput (0, [ AtomString [ Text "from.txt" ] ])
               ]
           }
       ; AtomOperand
           { invert = false
           ; env_vars = []
           ; cmd =
               [ SingleArg [ AtomString [ Text "ok" ] ]
               ; SingleArg [ AtomString [ Text "let's" ] ]
               ; SingleArg [ AtomString [ Text "go" ] ]
               ]
           ; redirs = [ DupInput (0, [ AtomString [ Text "from.txt" ] ]) ]
           }
       ])
;;

let%test _ =
  ok_pipe
    {|$((1+2))<&from.txt|}
    (Operands
       [ AtomOperand
           { invert = false
           ; env_vars = []
           ; cmd =
               [ SingleArg [ AtomString [ ArithmExpansion (Plus (Number 1, Number 2)) ] ]
               ]
           ; redirs = [ DupInput (0, [ AtomString [ Text "from.txt" ] ]) ]
           }
       ])
;;

let%test _ =
  ok_pipe
    {|$((1+2))<&from.txt<what.txt|}
    (Operands
       [ AtomOperand
           { invert = false
           ; env_vars = []
           ; cmd =
               [ SingleArg [ AtomString [ ArithmExpansion (Plus (Number 1, Number 2)) ] ]
               ]
           ; redirs =
               [ DupInput (0, [ AtomString [ Text "from.txt" ] ])
               ; RedirInput (0, [ AtomString [ Text "what.txt" ] ])
               ]
           }
       ])
;;

let%test _ =
  ok_pipe
    {|$(echo hello)|}
    (Operands
       [ AtomOperand
           { invert = false
           ; env_vars = []
           ; cmd =
               [ SingleArg
                   [ AtomString
                       [ CommandSubstitution
                           (Operands
                              [ AtomOperand
                                  { invert = false
                                  ; env_vars = []
                                  ; cmd =
                                      [ SingleArg [ AtomString [ Text "echo" ] ]
                                      ; SingleArg [ AtomString [ Text "hello" ] ]
                                      ]
                                  ; redirs = []
                                  }
                              ])
                       ]
                   ]
               ]
           ; redirs = []
           }
       ])
;;

let%test _ =
  ok_pipe
    {|$(echo $(echo hello))|}
    (Operands
       [ AtomOperand
           { invert = false
           ; env_vars = []
           ; cmd =
               [ SingleArg
                   [ AtomString
                       [ CommandSubstitution
                           (Operands
                              [ AtomOperand
                                  { invert = false
                                  ; env_vars = []
                                  ; cmd =
                                      [ SingleArg [ AtomString [ Text "echo" ] ]
                                      ; SingleArg
                                          [ AtomString
                                              [ CommandSubstitution
                                                  (Operands
                                                     [ AtomOperand
                                                         { invert = false
                                                         ; env_vars = []
                                                         ; cmd =
                                                             [ SingleArg
                                                                 [ AtomString
                                                                     [ Text "echo" ]
                                                                 ]
                                                             ; SingleArg
                                                                 [ AtomString
                                                                     [ Text "hello" ]
                                                                 ]
                                                             ]
                                                         ; redirs = []
                                                         }
                                                     ])
                                              ]
                                          ]
                                      ]
                                  ; redirs = []
                                  }
                              ])
                       ]
                   ]
               ]
           ; redirs = []
           }
       ])
;;

let%test _ = fail_pipe {|echo || pritn)|}

(* Loop parser tests *)

let ok_loop = test_ok pp_loop (loop_p ())
let fail_loop = test_fail pp_loop (loop_p ())

let%test _ =
  ok_loop
    {|for ((i=2;i<=num;i=i+1));
do
  fact=$((fact * i))
done|}
    (ForCompound
       { head =
           ArithmTriple
             ( Assignment ({ name = "i"; subscript = "0" }, Number 2)
             , LessOrEqual
                 ( Variable { name = "i"; subscript = "0" }
                 , Variable { name = "num"; subscript = "0" } )
             , Assignment
                 ( { name = "i"; subscript = "0" }
                 , Plus (Variable { name = "i"; subscript = "0" }, Number 1) ) )
       ; commands =
           [ Simple
               (Operands
                  [ AtomOperand
                      { invert = false
                      ; env_vars =
                          [ AtomVariable
                              { key = { name = "fact"; subscript = "0" }
                              ; value =
                                  [ AtomString
                                      [ ArithmExpansion
                                          (Asterisk
                                             ( Variable { name = "fact"; subscript = "0" }
                                             , Variable { name = "i"; subscript = "0" } ))
                                      ]
                                  ]
                              }
                          ]
                      ; cmd = []
                      ; redirs = []
                      }
                  ])
           ]
       })
;;

let%test _ =
  ok_loop
    {|until ((i>10));
    do
      i=$((i+1))
    done|}
    (UntilCompound
       { condition =
           [ Compound
               ( ArithmCompound
                   (Greater (Variable { name = "i"; subscript = "0" }, Number 10))
               , [] )
           ]
       ; cons_cmds =
           [ Simple
               (Operands
                  [ AtomOperand
                      { invert = false
                      ; env_vars =
                          [ AtomVariable
                              { key = { name = "i"; subscript = "0" }
                              ; value =
                                  [ AtomString
                                      [ ArithmExpansion
                                          (Plus
                                             ( Variable { name = "i"; subscript = "0" }
                                             , Number 1 ))
                                      ]
                                  ]
                              }
                          ]
                      ; cmd = []
                      ; redirs = []
                      }
                  ])
           ]
       })
;;

let%test _ =
  ok_loop
    {|while ((i<3));
    do
      echo=$e
    done|}
    (WhileCompound
       { condition =
           [ Compound
               ( ArithmCompound (Less (Variable { name = "i"; subscript = "0" }, Number 3))
               , [] )
           ]
       ; cons_cmds =
           [ Simple
               (Operands
                  [ AtomOperand
                      { invert = false
                      ; env_vars =
                          [ AtomVariable
                              { key = { name = "echo"; subscript = "0" }
                              ; value =
                                  [ AtomString [ ParamExpansion (VarNameExpansion "e") ] ]
                              }
                          ]
                      ; cmd = []
                      ; redirs = []
                      }
                  ])
           ]
       })
;;

let%test _ =
  ok_loop
    {|for e in some what;
    do
      echo=$e
    done|}
    (ForCompound
       { head =
           WordsIn
             ( "e"
             , [ SingleArg [ AtomString [ Text "some" ] ]
               ; SingleArg [ AtomString [ Text "what" ] ]
               ] )
       ; commands =
           [ Simple
               (Operands
                  [ AtomOperand
                      { invert = false
                      ; env_vars =
                          [ AtomVariable
                              { key = { name = "echo"; subscript = "0" }
                              ; value =
                                  [ AtomString [ ParamExpansion (VarNameExpansion "e") ] ]
                              }
                          ]
                      ; cmd = []
                      ; redirs = []
                      }
                  ])
           ]
       })
;;

let%test _ =
  ok_loop
    {|for eomw in esho{1..2}
do
   echo "Welcome times"
done|}
    (ForCompound
       { head =
           WordsIn
             ( "eomw"
             , [ MultipleArgs
                   [ [ AtomString [ Text "esho" ]; AtomString [ Text "1" ] ]
                   ; [ AtomString [ Text "esho" ]; AtomString [ Text "2" ] ]
                   ]
               ] )
       ; commands =
           [ Simple
               (Operands
                  [ AtomOperand
                      { invert = false
                      ; env_vars = []
                      ; cmd =
                          [ SingleArg [ AtomString [ Text "echo" ] ]
                          ; SingleArg [ DoubleQuotedString [ Text "Welcome times" ] ]
                          ]
                      ; redirs = []
                      }
                  ])
           ]
       })
;;

let%test _ =
  ok_loop
    {|for (( i=1; i <= 10; i=i+1 ));
  do;
  echo "number is"
  done|}
    (ForCompound
       { head =
           ArithmTriple
             ( Assignment ({ name = "i"; subscript = "0" }, Number 1)
             , LessOrEqual (Variable { name = "i"; subscript = "0" }, Number 10)
             , Assignment
                 ( { name = "i"; subscript = "0" }
                 , Plus (Variable { name = "i"; subscript = "0" }, Number 1) ) )
       ; commands =
           [ Simple
               (Operands
                  [ AtomOperand
                      { invert = false
                      ; env_vars = []
                      ; cmd =
                          [ SingleArg [ AtomString [ Text "echo" ] ]
                          ; SingleArg [ DoubleQuotedString [ Text "number is" ] ]
                          ]
                      ; redirs = []
                      }
                  ])
           ]
       })
;;

let%test _ =
  ok_loop
    {|while ((133));
    do
     echo hello
    done|}
    (WhileCompound
       { condition = [ Compound (ArithmCompound (Number 133), []) ]
       ; cons_cmds =
           [ Simple
               (Operands
                  [ AtomOperand
                      { invert = false
                      ; env_vars = []
                      ; cmd =
                          [ SingleArg [ AtomString [ Text "echo" ] ]
                          ; SingleArg [ AtomString [ Text "hello" ] ]
                          ]
                      ; redirs = []
                      }
                  ])
           ]
       })
;;

let%test _ =
  ok_loop
    {|while some && echo 1337;
    do
     echo; hello2;
    done|}
    (WhileCompound
       { condition =
           [ Simple
               (Operands
                  [ AndOperand
                      ( AtomOperand
                          { invert = false
                          ; env_vars = []
                          ; cmd = [ SingleArg [ AtomString [ Text "some" ] ] ]
                          ; redirs = []
                          }
                      , AtomOperand
                          { invert = false
                          ; env_vars = []
                          ; cmd =
                              [ SingleArg [ AtomString [ Text "echo" ] ]
                              ; SingleArg [ AtomString [ Text "1337" ] ]
                              ]
                          ; redirs = []
                          } )
                  ])
           ]
       ; cons_cmds =
           [ Simple
               (Operands
                  [ AtomOperand
                      { invert = false
                      ; env_vars = []
                      ; cmd = [ SingleArg [ AtomString [ Text "echo" ] ] ]
                      ; redirs = []
                      }
                  ])
           ; Simple
               (Operands
                  [ AtomOperand
                      { invert = false
                      ; env_vars = []
                      ; cmd = [ SingleArg [ AtomString [ Text "hello2" ] ] ]
                      ; redirs = []
                      }
                  ])
           ]
       })
;;

(* Other compounds parser tests *)

let ok_compound = test_ok pp_compound (compound_p ())
let fail_compound = test_fail pp_compound (compound_p ())

let%test _ =
  ok_compound
    {|{
  echo hello;
  echo hello2
}|}
    (Group
       [ Simple
           (Operands
              [ AtomOperand
                  { invert = false
                  ; env_vars = []
                  ; cmd =
                      [ SingleArg [ AtomString [ Text "echo" ] ]
                      ; SingleArg [ AtomString [ Text "hello" ] ]
                      ]
                  ; redirs = []
                  }
              ])
       ; Simple
           (Operands
              [ AtomOperand
                  { invert = false
                  ; env_vars = []
                  ; cmd =
                      [ SingleArg [ AtomString [ Text "echo" ] ]
                      ; SingleArg [ AtomString [ Text "hello2" ] ]
                      ]
                  ; redirs = []
                  }
              ])
       ])
;;

let%test _ =
  ok_compound
    {|{echo hello;}|}
    (Group
       [ Simple
           (Operands
              [ AtomOperand
                  { invert = false
                  ; env_vars = []
                  ; cmd =
                      [ SingleArg [ AtomString [ Text "echo" ] ]
                      ; SingleArg [ AtomString [ Text "hello" ] ]
                      ]
                  ; redirs = []
                  }
              ])
       ])
;;

let%test _ =
  ok_compound
    {|{
      echo hello
      }|}
    (Group
       [ Simple
           (Operands
              [ AtomOperand
                  { invert = false
                  ; env_vars = []
                  ; cmd =
                      [ SingleArg [ AtomString [ Text "echo" ] ]
                      ; SingleArg [ AtomString [ Text "hello" ] ]
                      ]
                  ; redirs = []
                  }
              ])
       ])
;;

let%test _ = fail_compound {|{echo hello}|}

let%test _ =
  ok_compound
    {|if ((1>2));
      then
        a=10
      fi|}
    (IfCompound
       [ { condition = [ Compound (ArithmCompound (Greater (Number 1, Number 2)), []) ]
         ; cons_cmds =
             [ Simple
                 (Operands
                    [ AtomOperand
                        { invert = false
                        ; env_vars =
                            [ AtomVariable
                                { key = { name = "a"; subscript = "0" }
                                ; value = [ AtomString [ Text "10" ] ]
                                }
                            ]
                        ; cmd = []
                        ; redirs = []
                        }
                    ])
             ]
         }
       ])
;;

let%test _ =
  ok_compound
    {|if ((1>2));
      then
        a=10
      else
        b=$a
      fi|}
    (IfCompound
       [ { condition = [ Compound (ArithmCompound (Greater (Number 1, Number 2)), []) ]
         ; cons_cmds =
             [ Simple
                 (Operands
                    [ AtomOperand
                        { invert = false
                        ; env_vars =
                            [ AtomVariable
                                { key = { name = "a"; subscript = "0" }
                                ; value = [ AtomString [ Text "10" ] ]
                                }
                            ]
                        ; cmd = []
                        ; redirs = []
                        }
                    ])
             ]
         }
       ; { condition = [ Compound (ArithmCompound (Number 1), []) ]
         ; cons_cmds =
             [ Simple
                 (Operands
                    [ AtomOperand
                        { invert = false
                        ; env_vars =
                            [ AtomVariable
                                { key = { name = "b"; subscript = "0" }
                                ; value =
                                    [ AtomString [ ParamExpansion (VarNameExpansion "a") ]
                                    ]
                                }
                            ]
                        ; cmd = []
                        ; redirs = []
                        }
                    ])
             ]
         }
       ])
;;

let%test _ =
  ok_compound
    {|if echo;
      then
        echo
      elif hz; then
        echo228
      elif hz; then
        echo228
      else
        echo what
      fi|}
    (IfCompound
       [ { condition =
             [ Simple
                 (Operands
                    [ AtomOperand
                        { invert = false
                        ; env_vars = []
                        ; cmd = [ SingleArg [ AtomString [ Text "echo" ] ] ]
                        ; redirs = []
                        }
                    ])
             ]
         ; cons_cmds =
             [ Simple
                 (Operands
                    [ AtomOperand
                        { invert = false
                        ; env_vars = []
                        ; cmd = [ SingleArg [ AtomString [ Text "echo" ] ] ]
                        ; redirs = []
                        }
                    ])
             ]
         }
       ; { condition =
             [ Simple
                 (Operands
                    [ AtomOperand
                        { invert = false
                        ; env_vars = []
                        ; cmd = [ SingleArg [ AtomString [ Text "hz" ] ] ]
                        ; redirs = []
                        }
                    ])
             ]
         ; cons_cmds =
             [ Simple
                 (Operands
                    [ AtomOperand
                        { invert = false
                        ; env_vars = []
                        ; cmd = [ SingleArg [ AtomString [ Text "echo228" ] ] ]
                        ; redirs = []
                        }
                    ])
             ]
         }
       ; { condition =
             [ Simple
                 (Operands
                    [ AtomOperand
                        { invert = false
                        ; env_vars = []
                        ; cmd = [ SingleArg [ AtomString [ Text "hz" ] ] ]
                        ; redirs = []
                        }
                    ])
             ]
         ; cons_cmds =
             [ Simple
                 (Operands
                    [ AtomOperand
                        { invert = false
                        ; env_vars = []
                        ; cmd = [ SingleArg [ AtomString [ Text "echo228" ] ] ]
                        ; redirs = []
                        }
                    ])
             ]
         }
       ; { condition = [ Compound (ArithmCompound (Number 1), []) ]
         ; cons_cmds =
             [ Simple
                 (Operands
                    [ AtomOperand
                        { invert = false
                        ; env_vars = []
                        ; cmd =
                            [ SingleArg [ AtomString [ Text "echo" ] ]
                            ; SingleArg [ AtomString [ Text "what" ] ]
                            ]
                        ; redirs = []
                        }
                    ])
             ]
         }
       ])
;;

let%test _ = fail_compound {|if echo;
      then
        echo
      |}

let%test _ = fail_compound {|if echo;
else
   echo
   fi|}

let%test _ =
  fail_compound {|if echo;
   then
      echo
   else
    echo
   else
    echo
   fi|}
;;

let%test _ =
  ok_compound
    {|case ${LO} in
      lo|fo) echo -n "four";;
      *|ro) echo hello;;
      esac|}
    (CaseIn
       { by =
           [ AtomString [ ParamExpansion (VarExpansion { name = "LO"; subscript = "0" }) ]
           ]
       ; cases =
           [ ( [ [ AtomString [ Text "lo" ] ]; [ AtomString [ Text "fo" ] ] ]
             , Simple
                 (Operands
                    [ AtomOperand
                        { invert = false
                        ; env_vars = []
                        ; cmd =
                            [ SingleArg [ AtomString [ Text "echo" ] ]
                            ; SingleArg [ AtomString [ Text "-n" ] ]
                            ; SingleArg [ DoubleQuotedString [ Text "four" ] ]
                            ]
                        ; redirs = []
                        }
                    ]) )
           ; ( [ [ AtomString [ Text "*" ] ]; [ AtomString [ Text "ro" ] ] ]
             , Simple
                 (Operands
                    [ AtomOperand
                        { invert = false
                        ; env_vars = []
                        ; cmd =
                            [ SingleArg [ AtomString [ Text "echo" ] ]
                            ; SingleArg [ AtomString [ Text "hello" ] ]
                            ]
                        ; redirs = []
                        }
                    ]) )
           ]
       })
;;

let%test _ =
  ok_compound
    {|case ${LO} in
      lo|fo) echo -n "four";;
      ro) echo hello;;
      esac|}
    (CaseIn
       { by =
           [ AtomString [ ParamExpansion (VarExpansion { name = "LO"; subscript = "0" }) ]
           ]
       ; cases =
           [ ( [ [ AtomString [ Text "lo" ] ]; [ AtomString [ Text "fo" ] ] ]
             , Simple
                 (Operands
                    [ AtomOperand
                        { invert = false
                        ; env_vars = []
                        ; cmd =
                            [ SingleArg [ AtomString [ Text "echo" ] ]
                            ; SingleArg [ AtomString [ Text "-n" ] ]
                            ; SingleArg [ DoubleQuotedString [ Text "four" ] ]
                            ]
                        ; redirs = []
                        }
                    ]) )
           ; ( [ [ AtomString [ Text "ro" ] ] ]
             , Simple
                 (Operands
                    [ AtomOperand
                        { invert = false
                        ; env_vars = []
                        ; cmd =
                            [ SingleArg [ AtomString [ Text "echo" ] ]
                            ; SingleArg [ AtomString [ Text "hello" ] ]
                            ]
                        ; redirs = []
                        }
                    ]) )
           ]
       })
;;

let%test _ =
  fail_compound
    {|case ${LO} in
      |lo|fo) echo -n "four";;
      ro) echo hello;;
      esac|}
;;

let%test _ =
  fail_compound
    {|case ${LO} in ;
        lo|fo) echo -n "four";;
        ro) echo hello;;
        esac|}
;;

(* Functions parser tests *)

let ok_fn = test_ok pp_command_or_func (parse_function ())
let fail_fn = test_fail pp_command_or_func (parse_function ())

let%test _ =
  ok_fn
    {|function what () {
  a1=5
}|}
    (Func
       { name = "what"
       ; body =
           [ Command
               (Simple
                  (Operands
                     [ AtomOperand
                         { invert = false
                         ; env_vars =
                             [ AtomVariable
                                 { key = { name = "a1"; subscript = "0" }
                                 ; value = [ AtomString [ Text "5" ] ]
                                 }
                             ]
                         ; cmd = []
                         ; redirs = []
                         }
                     ]))
           ]
       })
;;

let%test _ =
  ok_fn
    {|function name () {
      echo hello
}|}
    (Func
       { name = "name"
       ; body =
           [ Command
               (Simple
                  (Operands
                     [ AtomOperand
                         { invert = false
                         ; env_vars = []
                         ; cmd =
                             [ SingleArg [ AtomString [ Text "echo" ] ]
                             ; SingleArg [ AtomString [ Text "hello" ] ]
                             ]
                         ; redirs = []
                         }
                     ]))
           ]
       })
;;

let%test _ =
  ok_fn
    {|function fn1 () {
      some=$((i = j))
      some=$((i < j))
    }|}
    (Func
       { name = "fn1"
       ; body =
           [ Command
               (Simple
                  (Operands
                     [ AtomOperand
                         { invert = false
                         ; env_vars =
                             [ AtomVariable
                                 { key = { name = "some"; subscript = "0" }
                                 ; value =
                                     [ AtomString
                                         [ ArithmExpansion
                                             (Assignment
                                                ( { name = "i"; subscript = "0" }
                                                , Variable { name = "j"; subscript = "0" }
                                                ))
                                         ]
                                     ]
                                 }
                             ]
                         ; cmd = []
                         ; redirs = []
                         }
                     ]))
           ; Command
               (Simple
                  (Operands
                     [ AtomOperand
                         { invert = false
                         ; env_vars =
                             [ AtomVariable
                                 { key = { name = "some"; subscript = "0" }
                                 ; value =
                                     [ AtomString
                                         [ ArithmExpansion
                                             (Less
                                                ( Variable { name = "i"; subscript = "0" }
                                                , Variable { name = "j"; subscript = "0" }
                                                ))
                                         ]
                                     ]
                                 }
                             ]
                         ; cmd = []
                         ; redirs = []
                         }
                     ]))
           ]
       })
;;

let%test _ =
  ok_fn
    {|function what () {
  a1=5
  b1=$((10+(227 * 328)))
}|}
    (Func
       { name = "what"
       ; body =
           [ Command
               (Simple
                  (Operands
                     [ AtomOperand
                         { invert = false
                         ; env_vars =
                             [ AtomVariable
                                 { key = { name = "a1"; subscript = "0" }
                                 ; value = [ AtomString [ Text "5" ] ]
                                 }
                             ]
                         ; cmd = []
                         ; redirs = []
                         }
                     ]))
           ; Command
               (Simple
                  (Operands
                     [ AtomOperand
                         { invert = false
                         ; env_vars =
                             [ AtomVariable
                                 { key = { name = "b1"; subscript = "0" }
                                 ; value =
                                     [ AtomString
                                         [ ArithmExpansion
                                             (Plus
                                                ( Number 10
                                                , Asterisk (Number 227, Number 328) ))
                                         ]
                                     ]
                                 }
                             ]
                         ; cmd = []
                         ; redirs = []
                         }
                     ]))
           ]
       })
;;

let%test _ =
  ok_fn
    {|function what () {
        a1=5
        function what2 () {
          a3=9
        }
        what2
      }|}
    (Func
       { name = "what"
       ; body =
           [ Command
               (Simple
                  (Operands
                     [ AtomOperand
                         { invert = false
                         ; env_vars =
                             [ AtomVariable
                                 { key = { name = "a1"; subscript = "0" }
                                 ; value = [ AtomString [ Text "5" ] ]
                                 }
                             ]
                         ; cmd = []
                         ; redirs = []
                         }
                     ]))
           ; Func
               { name = "what2"
               ; body =
                   [ Command
                       (Simple
                          (Operands
                             [ AtomOperand
                                 { invert = false
                                 ; env_vars =
                                     [ AtomVariable
                                         { key = { name = "a3"; subscript = "0" }
                                         ; value = [ AtomString [ Text "9" ] ]
                                         }
                                     ]
                                 ; cmd = []
                                 ; redirs = []
                                 }
                             ]))
                   ]
               }
           ; Command
               (Simple
                  (Operands
                     [ AtomOperand
                         { invert = false
                         ; env_vars = []
                         ; cmd = [ SingleArg [ AtomString [ Text "what2" ] ] ]
                         ; redirs = []
                         }
                     ]))
           ]
       })
;;

(* Script parser tests *)

let ok_script = test_ok pp_script (parse_script ())
let fail_script = test_fail pp_script (parse_script ())

let%test _ =
  ok_script
    {|echo hello >some.txt | echo hello|}
    [ Command
        (Simple
           (Operands
              [ AtomOperand
                  { invert = false
                  ; env_vars = []
                  ; cmd =
                      [ SingleArg [ AtomString [ Text "echo" ] ]
                      ; SingleArg [ AtomString [ Text "hello" ] ]
                      ]
                  ; redirs = [ RedirOutput (1, [ AtomString [ Text "some.txt" ] ]) ]
                  }
              ; AtomOperand
                  { invert = false
                  ; env_vars = []
                  ; cmd =
                      [ SingleArg [ AtomString [ Text "echo" ] ]
                      ; SingleArg [ AtomString [ Text "hello" ] ]
                      ]
                  ; redirs = []
                  }
              ]))
    ]
;;

let%test _ =
  ok_script
    {|ls >dirlist 2>&1|}
    [ Command
        (Simple
           (Operands
              [ AtomOperand
                  { invert = false
                  ; env_vars = []
                  ; cmd = [ SingleArg [ AtomString [ Text "ls" ] ] ]
                  ; redirs =
                      [ RedirOutput (1, [ AtomString [ Text "dirlist" ] ])
                      ; DupOutput (2, [ AtomString [ Text "1" ] ])
                      ]
                  }
              ]))
    ]
;;

let%test _ =
  ok_script
    {|echo hello && ((1+2)) |}
    [ Command
        (Simple
           (Operands
              [ AndOperand
                  ( AtomOperand
                      { invert = false
                      ; env_vars = []
                      ; cmd =
                          [ SingleArg [ AtomString [ Text "echo" ] ]
                          ; SingleArg [ AtomString [ Text "hello" ] ]
                          ]
                      ; redirs = []
                      }
                  , CompoundOperand (ArithmCompound (Plus (Number 1, Number 2))) )
              ]))
    ]
;;

let%test _ =
  ok_script
    {|((1+2)) && echo hello|}
    [ Command
        (Simple
           (Operands
              [ AndOperand
                  ( CompoundOperand (ArithmCompound (Plus (Number 1, Number 2)))
                  , AtomOperand
                      { invert = false
                      ; env_vars = []
                      ; cmd =
                          [ SingleArg [ AtomString [ Text "echo" ] ]
                          ; SingleArg [ AtomString [ Text "hello" ] ]
                          ]
                      ; redirs = []
                      } )
              ]))
    ]
;;

let%test _ =
  ok_script
    {|if (( 2 > 3 )) || (( 2 + 2 = 4 )) && (( 1 + 1 = 2 ))
      then echo should be printed 3
      else echo should not be printed
      fi|}
    [ Command
        (Compound
           ( IfCompound
               [ { condition =
                     [ Simple
                         (Operands
                            [ OrOperand
                                ( CompoundOperand
                                    (ArithmCompound (Greater (Number 2, Number 3)))
                                , AndOperand
                                    ( CompoundOperand
                                        (ArithmCompound
                                           (Equal (Plus (Number 2, Number 2), Number 4)))
                                    , CompoundOperand
                                        (ArithmCompound
                                           (Equal (Plus (Number 1, Number 1), Number 2)))
                                    ) )
                            ])
                     ]
                 ; cons_cmds =
                     [ Simple
                         (Operands
                            [ AtomOperand
                                { invert = false
                                ; env_vars = []
                                ; cmd =
                                    [ SingleArg [ AtomString [ Text "echo" ] ]
                                    ; SingleArg [ AtomString [ Text "should" ] ]
                                    ; SingleArg [ AtomString [ Text "be" ] ]
                                    ; SingleArg [ AtomString [ Text "printed" ] ]
                                    ; SingleArg [ AtomString [ Text "3" ] ]
                                    ]
                                ; redirs = []
                                }
                            ])
                     ]
                 }
               ; { condition = [ Compound (ArithmCompound (Number 1), []) ]
                 ; cons_cmds =
                     [ Simple
                         (Operands
                            [ AtomOperand
                                { invert = false
                                ; env_vars = []
                                ; cmd =
                                    [ SingleArg [ AtomString [ Text "echo" ] ]
                                    ; SingleArg [ AtomString [ Text "should" ] ]
                                    ; SingleArg [ AtomString [ Text "not" ] ]
                                    ; SingleArg [ AtomString [ Text "be" ] ]
                                    ; SingleArg [ AtomString [ Text "printed" ] ]
                                    ]
                                ; redirs = []
                                }
                            ])
                     ]
                 }
               ]
           , [] ))
    ]
;;
