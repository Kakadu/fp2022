open Luatypes
open Utils
open Ast

module Parser : sig
  type input

  type 'a parse_result =
    | Failed of string
    | Parsed of 'a * input
    | HardFailed of string

  type 'a parser

  val string_to_input : string -> input
  val input_to_string : input -> string
  val parse : input -> Ast.ast parse_result
  val parse_expr : input -> Ast.expr parse_result
end = struct
  include Ast

  type input = char list

  let string_to_input = explode
  let input_to_string = implode

  (* Parsing results *)
  type 'a parse_result =
    | Failed of string
    | Parsed of 'a * input
    | HardFailed of string

  type 'a parser = input -> 'a parse_result

  (* return parser *)
  let return x : _ parser = fun s -> Parsed (x, s)
  let fail message _ = Failed message
  let hardfail message _ = HardFailed message

  (* Parse char if cond returns true *)
  let parse_symbol cond = function
    | h :: t when cond h -> return h t
    | h :: _ -> Failed ("symbol \"" ^ String.make 1 h ^ "\" not resolved")
    | _ -> Failed "unexpected EOF"
  ;;

  let ( >>= ) p f s =
    match p s with
    | Failed msg -> Failed msg
    | HardFailed msg -> HardFailed msg
    | Parsed (h, t) -> f h t
  ;;

  let ( *> ) p1 p2 = p1 >>= fun _ -> p2
  let ( <* ) p1 p2 = p1 >>= fun h -> p2 *> return h

  let ( >> ) p1 p2 s =
    match p1 s with
    | Parsed (_, t) -> p2 t
    | _ -> p2 s
  ;;

  let ( !! ) par s =
    match par s with
    | Parsed (h, t) -> Parsed (h, t)
    | Failed m | HardFailed m -> HardFailed m
  ;;

  let ( -/-> ) par msg s =
    match par s with
    | Parsed (h, t) -> Parsed (h, t)
    | HardFailed m -> HardFailed m
    | Failed _ -> Failed msg
  ;;

  (* or operator *)
  let ( <|> ) p1 p2 s =
    match p1 s with
    | Failed _ -> p2 s
    | HardFailed msg -> HardFailed msg
    | res -> res
  ;;

  let ( << ) p1 p2 s = (p1 <* p2 <|> p1) s

  (* if the parser fails will return None, else ruturs Some 'a *)
  let wrap p i =
    match p i with
    | Parsed (h, t) -> return (Some h) t
    | Failed _ | HardFailed _ -> return None i
  ;;

  let fail_if_parsed p inp =
    match p inp with
    | Parsed (_, _) -> Failed "success"
    | Failed _ | HardFailed _ -> return () inp
  ;;

  (* Parses many that are parsed by parser given *)
  let parse_many : 'a parser -> 'a list parser =
   fun p ->
    let rec helper s =
      match p s with
      | Failed _ | HardFailed _ -> return [] s
      | Parsed (h, t) -> (helper >>= fun xs -> return (h :: xs)) t
    in
    helper
 ;;

  (* Parses given sequence of chars*)
  let parse_sequence seq code =
    let rec helper partseq = function
      | [] -> Failed ("exprected " ^ implode seq)
      | codeh :: t1 ->
        (match partseq with
         | [] -> Parsed (seq, code)
         | seqh :: [] when seqh = codeh -> Parsed (seq, t1)
         | seqh :: t2 when seqh = codeh -> helper t2 t1
         | _ :: _ -> Failed ("exprected " ^ implode seq))
    in
    helper seq code
  ;;

  (* Parses spaces *)
  let parse_spaces =
    parse_many (parse_symbol (fun x -> x = ' ' || x = '\t' || x = '\n'))
    >>= fun x -> return (Spaces (implode x))
  ;;

  (* Parses parentheses *)
  let parse_l_par = parse_symbol (fun x -> x = '(') *> return LParentheses
  let parse_r_par = parse_symbol (fun x -> x = ')') *> return RParentheses
  let parse_l_br = parse_symbol (fun x -> x = '[') *> return LBrackets
  let parse_r_br = parse_symbol (fun x -> x = ']') *> return RBrackets
  let parse_l_c_br = parse_symbol (fun x -> x = '{') *> return LCurlBrackets
  let parse_r_c_br = parse_symbol (fun x -> x = '}') *> return RCurlBrackets

  (* Parses komma *)
  let parse_comma = parse_symbol (fun x -> x = ',') *> return Comma

  (* Parses dot *)
  let parse_dot = parse_symbol (fun x -> x = '.') *> return Dot

  (* Parses semicolon *)
  let parse_semicolon = parse_symbol (fun x -> x = ';') *> return Semicolon

  (* Parses one line comment *)
  let parse_comment =
    parse_sequence [ '-'; '-' ] *> parse_many (parse_symbol (fun x -> x != '\n'))
    >>= fun c -> return (Comment (implode c))
  ;;

  (* Parses variable or keyword *)
  let parse_variable_or_keyword =
    parse_symbol (fun x -> isChar x || x = '_')
    >>= fun first ->
    parse_many (parse_symbol (fun x1 -> isChar x1 || isDigit x1 || x1 = '_'))
    >>= fun left ->
    let full = first :: left in
    if List.mem full (List.map explode luakeywords)
    then return (Keyword (implode full))
    else return (Variable (implode full))
  ;;

  (* Parses keyword *)
  let parse_keyword =
    parse_variable_or_keyword
    >>= fun res ->
    match res with
    | Keyword k -> return (Keyword k)
    | _ -> fail "not a keyword"
  ;;

  (* Parses variable *)
  let parse_variable =
    parse_variable_or_keyword
    >>= fun res ->
    match res with
    | Variable v -> return (Variable v)
    | _ -> fail "not a keyword"
  ;;

  (* return true if there is a dot, returns false otherwise*)
  let peek_parse p inp =
    match p inp with
    | Failed _ | HardFailed _ -> Parsed (false, inp)
    | Parsed _ -> Parsed (true, inp)
  ;;

  (* Parses integers*)
  let parse_number =
    parse_many (parse_symbol isDigit)
    >>= fun whole ->
    match whole with
    | [] -> fail "Not a number"
    | _ ->
      peek_parse parse_dot
      >>= (function
      | true ->
        parse_dot *> parse_many (parse_symbol isDigit)
        >>= fun part -> return (Number (implode whole ^ "." ^ implode part))
      | false -> return (Number (implode whole)))
  ;;

  (* Parse operator (sequence of +-=<>/%~*^ chars)*)
  let parse_operator =
    (fun p ->
      match
        parse_many
          (parse_symbol (fun x ->
             List.mem x [ '='; '+'; '-'; '*'; '/'; '%'; '^'; '~'; '<'; '>' ]))
          p
      with
      | Parsed ([], _) -> Failed "not an operator"
      | Parsed (res, t) -> return (Operator (implode res)) t
      | Failed msg | HardFailed msg -> Failed msg)
    <|> (parse_sequence (explode "and")
        <|> parse_sequence (explode "or")
        <|> parse_sequence (explode "not")
        <|> parse_sequence (explode "..")
        >>= fun res -> return (Operator (implode res)))
  ;;

  (* Parses string*)
  let parse_string ~begin_parser ~end_parser =
    begin_parser *> parse_many (fail_if_parsed end_parser *> parse_symbol (fun _ -> true))
    <* end_parser
    >>= fun s -> return (String (implode s))
  ;;

  (* Takes code and position, and gets line number and character offset *)
  let get_char_position code position =
    let string_until = String.sub code 0 position in
    let splitted_string = String.split_on_char '\n' string_until in
    ( List.length splitted_string
    , List.length (explode (List.hd (List.rev splitted_string))) )
  ;;

  (**** Parsers that skip useless stuff ****)
  let parse_useless_stuff =
    let parse_any_spaces =
      parse_spaces
      >>= function
      | Spaces "" -> fail "no spaces"
      | s -> return s
    in
    parse_many (parse_any_spaces <|> parse_comment <|> parse_comment)
  ;;

  let s_parse_comma = parse_useless_stuff >> parse_comma -/-> "Expected ,"
  let s_parse_dot = parse_useless_stuff >> parse_dot -/-> "Expected ."
  let s_parse_l_bracket = parse_useless_stuff >> parse_l_br -/-> "Expected ["
  let s_parse_r_bracket = parse_useless_stuff >> parse_r_br -/-> "Expected ]"
  let s_parse_l_curly_bracket = parse_useless_stuff >> parse_l_c_br -/-> "Expected {"
  let s_parse_r_curly_bracket = parse_useless_stuff >> parse_r_c_br -/-> "Expected }"
  let s_parse_l_par = parse_useless_stuff >> parse_l_par -/-> "Expected ("
  let s_parse_r_par = parse_useless_stuff >> parse_r_par -/-> "Expected )"
  let s_parse_semicolon = parse_useless_stuff >> parse_semicolon -/-> "Expected ;"
  let s_parse_number = parse_useless_stuff >> parse_number -/-> "Expected number"
  let s_parse_operator = parse_useless_stuff >> parse_operator -/-> "Expected operator"

  let s_parse_variable =
    parse_useless_stuff >> parse_variable -/-> "Expected variable ident "
  ;;

  let s_parse_keyword = parse_useless_stuff >> parse_keyword -/-> "Expected keyword "

  let s_parse_string =
    let pse s = parse_sequence (explode s) in
    parse_useless_stuff
    >> (parse_string ~begin_parser:(pse "'") ~end_parser:(pse "'")
       <|> parse_string ~begin_parser:(pse "\"") ~end_parser:(pse "\"")
       <|> parse_string ~begin_parser:(pse "[[") ~end_parser:(pse "]]"))
  ;;

  let s_parse_arithm_operator =
    s_parse_operator
    >>= function
    | Operator x when List.mem x operators -> return (Operator x)
    | _ -> fail "Expected arithmetc operator"
  ;;

  let parse_specific_keyword kw inp =
    (s_parse_keyword
    >>= function
    | Keyword w when w = kw -> return true
    | _ -> fail ("Expected " ^ kw))
      inp
  ;;

  (**** Makes ast tree ****)
  (* parses variable and returns Id string*)
  let parse_ident =
    s_parse_variable
    >>= function
    | Variable v -> return v
    | _ -> fail "ident expected"
  ;;

  (* parses '=' operator only *)
  let parse_assign_op =
    s_parse_operator
    >>= function
    | Operator "=" -> return (Operator "=")
    | _ -> fail "= expected"
  ;;

  (* parses bool as expression *)
  let parse_bool_expr =
    s_parse_keyword
    >>= function
    | Keyword "true" -> return (LuaConst (LuaBool true))
    | Keyword "false" -> return (LuaConst (LuaBool false))
    | _ -> fail "boolean expected"
  ;;

  (* parses string as expression *)
  let parse_string_expr =
    s_parse_string
    >>= function
    | String s -> return (LuaConst (LuaString s))
    | _ -> fail "string expected"
  ;;

  (* parses nil as expression *)
  let parse_nil_expr = parse_specific_keyword "nil" *> return (LuaConst LuaNil)

  (* parses number as expression *)
  let parse_number_expr =
    s_parse_number
    >>= function
    | Number n -> return (LuaConst (LuaNumber (float_of_string n)))
    | _ -> fail "number expected"
  ;;

  (* delim parser*)
  let delim_parser ~inner_parser ~sep_parser =
    parse_many (inner_parser <* sep_parser)
    >>= (fun others -> inner_parser >>= fun last -> return (others @ [ last ]))
    <|> return []
  ;;

  (* get binary operator precedence*)
  let get_presedence = function
    | "^" -> 60
    | "*" | "/" -> 40
    | "+" | "-" -> 20
    | ">" | "<" -> 10
    | ".." -> 70
    | _ -> 0
  ;;

  (* parse expression *)
  let rec parse_expr inp = (parse_bin_op_expr <|> parse_primary_expr) inp

  (* parse expression but not bin op*)
  and parse_primary_expr inp =
    ((parse_const
     <|> parse_par_expr
     <|> parse_var_expr
     <|> parse_table_init
     <|> parse_unary_op
     >>= fun body t ->
     let rec helper b t =
       match (parse_func_call b <|> parse_table_access b) t with
       | Failed _ | HardFailed _ -> Parsed (b, t)
       | Parsed (v, t1) -> helper v t1
     in
     helper body t)
    -/-> "Expected expression")
      inp

  (* parses expr of a constant
   TODO: Add other const types
*)
  and parse_const inp =
    (parse_string_expr
    <|> parse_number_expr
    <|> parse_bool_expr
    <|> parse_nil_expr
    <|> parse_func_const)
      inp

  and parse_unary_op inp =
    (s_parse_arithm_operator
    >>= function
    | Operator op -> !!parse_primary_expr >>= fun e -> return (LuaUnOp (op, e))
    | _ -> fail "expected operator")
      inp

  (* parses binary operators with precedence*)
  and parse_bin_op_rhs expr_recedence lhs inp =
    match s_parse_arithm_operator inp with
    | Parsed (Operator op1, t1) ->
      let this_precedence = get_presedence op1 in
      if this_precedence < expr_recedence
      then Parsed (lhs, inp)
      else (
        match parse_primary_expr t1 with
        | Parsed (rPrExpr, t2) ->
          let rhs =
            match s_parse_arithm_operator t2 with
            | Parsed (Operator op2, _) ->
              let next_precedence = get_presedence op2 in
              if this_precedence < next_precedence
              then parse_bin_op_rhs (this_precedence + 1) rPrExpr t2
              else Parsed (rPrExpr, t2)
            | _ -> Parsed (rPrExpr, t2)
          in
          (match rhs with
           | Parsed (rhs, t4) ->
             let lhs = LuaBinOp (op1, lhs, rhs) in
             parse_bin_op_rhs expr_recedence lhs t4
           | Failed m | HardFailed m -> Failed m)
        | _ -> HardFailed "expected expression after operator")
    | _ -> Parsed (lhs, inp)

  (* parses expression in parentheses*)
  and parse_par_expr inp = (s_parse_l_par *> parse_expr <* !!s_parse_r_par) inp

  (* parses bin operator expr*)
  and parse_bin_op_expr inp =
    (parse_primary_expr >>= fun lhs -> parse_bin_op_rhs 0 lhs) inp

  (* parses variable *)
  and parse_var_expr inp =
    (s_parse_variable
    >>= function
    | Variable v -> return (LuaVariable v)
    | _ -> fail "variable expected")
      inp

  (* parse (arg1, arg2...) and takes the first arg as the body *)
  and parse_func_call func_target inp =
    (s_parse_l_par *> delim_parser ~inner_parser:parse_expr ~sep_parser:s_parse_comma
    <* !!s_parse_r_par
    >>= fun args ->
    return (LuaCall (func_target, args)) >>= fun apply -> return (LuaExprApply apply))
      inp

  (* parse table access expr[expr]*)
  and parse_table_access access_target inp =
    (s_parse_l_bracket *> parse_expr
    <* !!s_parse_r_bracket
    >>= (fun index -> return (LuaTableGet (access_target, index)))
    <|> (s_parse_dot *> !!parse_ident
        >>= fun index -> return (LuaTableGet (access_target, LuaConst (LuaString index)))
        ))
      inp

  (* parse table init {expr, expr, ...}*)
  and parse_table_init inp =
    let parse_kv_pair_or_value =
      s_parse_l_bracket *> parse_expr
      <* !!s_parse_r_bracket
      <* parse_assign_op
      >>= (fun key -> parse_expr >>= fun value -> return (PairExpr (key, value)))
      <|> (parse_expr >>= fun e -> return (JustExpr e))
    in
    (s_parse_l_curly_bracket
     *> delim_parser ~inner_parser:parse_kv_pair_or_value ~sep_parser:s_parse_comma
    <* s_parse_r_curly_bracket
    >>= fun contents -> return (LuaTableInit contents))
      inp

  (* parses function(a, b) type statement*)
  and parse_func_const inp =
    (parse_specific_keyword "function"
     *> s_parse_l_par
     *> delim_parser ~inner_parser:parse_ident ~sep_parser:s_parse_comma
    <* !!s_parse_r_par
    >>= fun args ->
    parse_block !!parse_end >>= fun body -> return (LuaConst (LuaFunction (args, body))))
      inp

  (**** Parses statements ****)
  and parse_statement inp =
    (parse_if
    <|> parse_assign
    <|> parse_while
    <|> parse_do_block_wrapped
    <|> parse_return
    <|> parse_local
    <|> parse_repeat
    <|> parse_for_num
    <|> parse_for_in
    <|> parse_func_call_statement
    <|> parse_break
    <|> parse_function_declare
    <|> parse_stat_expr)
      inp

  (* parses expr as a statement for REPL *)
  and parse_stat_expr inp = (parse_expr >>= fun e -> return (LuaExpr e)) inp

  (* parses statements until end statement*)
  and parse_block end_parser inp =
    (parse_many (fail_if_parsed end_parser *> parse_statement) <* !!end_parser) inp

  (* parses statements until fail and until end_parser *)
  and parse_block_until_fail end_parser inp =
    (parse_many parse_statement <* end_parser) inp

  (* parses statements until fail and until end_parser but not eating last kw*)
  and parse_block_until_kwd end_parser inp =
    (parse_many parse_statement
    <* fun x ->
    match peek_parse end_parser x with
    | Parsed (true, o) -> Parsed (true, o)
    | _ -> Failed "expected end of block")
      inp

  (* parses lvalue, ident or lval[expr]*)
  and parse_lvalue inp =
    ((parse_ident
     >>= fun ident ->
     parse_many
       (s_parse_l_bracket *> parse_expr
       <* !!s_parse_r_bracket
       <|> (parse_dot *> !!parse_ident
           >>= fun index -> return (LuaConst (LuaString index))))
     >>= fun table_accesses ->
     let rec form_lvalue initial = function
       | [] -> initial
       | h :: t -> form_lvalue (Index (initial, h)) t
     in
     return (form_lvalue (Ident ident) table_accesses))
    -/-> "Expected lvalue")
      inp

  (* parses assign a, b, c = d, e, f *)
  and parse_assign inp =
    (let get_targets =
       delim_parser ~inner_parser:parse_lvalue ~sep_parser:s_parse_comma
     in
     let get_args = delim_parser ~inner_parser:parse_expr ~sep_parser:s_parse_comma in
     get_targets
     >>= fun lhs ->
     s_parse_operator
     >>= function
     | Operator "=" -> get_args >>= fun rhs -> return (LuaSet (lhs, rhs))
     | _ -> fail "assignment expected")
      inp

  (* parses if statement *)
  and parse_if inp =
    (let parse_elseif =
       parse_specific_keyword "elseif" *> !!parse_expr
       >>= fun elseif_cond ->
       !!(parse_specific_keyword "then") *> !!(parse_block_until_kwd (return true))
       >>= fun elseif_block -> return (elseif_cond, elseif_block)
     in
     let parse_else =
       parse_specific_keyword "else" *> !!(parse_block_until_fail parse_end)
       >>= fun else_block -> return else_block
     in
     parse_specific_keyword "if" *> !!parse_expr
     >>= fun condition ->
     !!(parse_specific_keyword "then")
     *> (!!(parse_block_until_kwd
              (parse_end
              <|> parse_specific_keyword "else"
              <|> parse_specific_keyword "elseif")
           << parse_end)
        -/-> "Expected block after then")
     >>= fun if_block ->
     parse_many parse_elseif
     >>= fun elseif_blocks ->
     wrap parse_else
     >>= fun else_block -> return (LuaIf (condition, if_block, elseif_blocks, else_block)))
      inp

  (* parses return statement*)
  and parse_return inp =
    (parse_specific_keyword "return" *> wrap parse_expr >>= fun e -> return (LuaReturn e))
      inp

  (* parses local variable creation *)
  and parse_local inp =
    (parse_specific_keyword "local"
     *> delim_parser ~inner_parser:parse_ident ~sep_parser:s_parse_comma
    >>= fun local_vars ->
    wrap parse_assign_op
    >>= function
    | Some _ ->
      delim_parser ~inner_parser:parse_expr ~sep_parser:s_parse_comma
      >>= fun assignments -> return (LuaLocal (local_vars, assignments))
    | None -> return (LuaLocal (local_vars, [])))
      inp

  (*parses while loop*)
  and parse_while inp =
    (parse_specific_keyword "while" *> !!parse_expr
    >>= fun e -> !!parse_do_block >>= fun do_block -> return (LuaWhile (e, do_block)))
      inp

  (* parses repeat loop *)
  and parse_repeat inp =
    (parse_specific_keyword "repeat" *> !!(parse_block (parse_specific_keyword "until"))
    >>= fun block -> parse_expr >>= fun ex -> return (LuaRepeat (block, ex)))
      inp

  (* parses for loop *)
  and parse_for_num inp =
    (parse_specific_keyword "for" *> parse_ident
    <* parse_assign_op
    >>= fun vname ->
    !!parse_expr
    >>= fun start_ex ->
    s_parse_comma *> !!parse_expr
    >>= fun stop_ex ->
    wrap (s_parse_comma *> parse_expr)
    >>= fun step_ex ->
    parse_do_block
    >>= fun bl -> return (LuaFornum (vname, start_ex, stop_ex, step_ex, bl)))
      inp

  (* a func call is also a statement due to side effects in func calls. Should be called after other *)
  and parse_func_call_statement inp =
    (parse_primary_expr
    >>= function
    | LuaExprApply e -> return (LuaStatementApply e)
    | _ -> fail "expected function call")
      inp

  (* parses statements starting with do and ending with end *)
  and parse_do_block inp = (parse_specific_keyword "do" *> !!(parse_block parse_end)) inp

  (* parses statements starting with do and ending with end but wrapped *)
  and parse_do_block_wrapped inp = (parse_do_block >>= fun bl -> return (LuaDo bl)) inp

  (* parses for in loop *)
  and parse_for_in inp =
    (parse_specific_keyword "for"
     *> delim_parser ~inner_parser:parse_ident ~sep_parser:s_parse_comma
    >>= fun fl_lhs ->
    parse_specific_keyword "in"
    *> delim_parser ~inner_parser:parse_expr ~sep_parser:s_parse_comma
    >>= fun fl_rhs ->
    parse_do_block >>= fun body -> return (LuaForin (fl_lhs, fl_rhs, body)))
      inp

  (* parses end keyword*)
  and parse_end = parse_specific_keyword "end"

  (* parses break in a loop *)
  and parse_break = parse_specific_keyword "break" *> return LuaBreak

  (* parses function declare*)
  and parse_function_declare inp =
    (parse_specific_keyword "function" *> parse_lvalue
    >>= fun name ->
    s_parse_l_par *> delim_parser ~inner_parser:parse_ident ~sep_parser:s_parse_comma
    <* s_parse_r_par
    >>= fun args ->
    parse_block parse_end >>= fun body -> return (LuaFunctionDeclare (name, args, body)))
      inp
  ;;

  (* Parses many that are parsed by parser given *)
  let parse_many_ex p =
    let rec helper s =
      match p s with
      | Failed m | HardFailed m -> fail m s
      | Parsed (h, []) -> return [ h ] []
      | Parsed (h, t) -> (helper >>= fun xs -> return (h :: xs)) t
    in
    helper
  ;;

  (* parses the program to an ast *)
  let parse = parse_many_ex (parse_statement << parse_useless_stuff)
end