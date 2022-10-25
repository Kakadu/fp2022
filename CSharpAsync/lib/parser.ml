open Ast
open Opal

let reserved =
  [
    "true";
    "false";
    "if";
    "else";
    "while";
    "public";
    "static";
    "const";
    "void";
    "string";
    "char";
    "int";
    "bool";
    "for";
    "null";
    "new";
    "return";
    "break";
    "continue";
    "class";
    "async";
    "await";
    "select";
    "from";
    "Console.WriteLine";
  ]

(** Basic parser functions **)

let const = token "const" >> return Const
let parens = between (token "(") (token ")")
let braces = between (token "{") (token "}")
let ar_braces = between (token "[") (token "]")

let modifier_list =
  many
    (choice
       [
         token "public" >> return Public;
         token "static" >> return Static;
         token "const" >> return Const;
         token "async" >> return Async;
       ])

let digits = spaces >> many1 digit => implode
let convert_to_int = digits => int_of_string

(** Expressions **)

module Expr = struct
  open Ast

  let op_add = token "+" >> return (fun x y -> Add (x, y))
  let op_sub = token "-" >> return (fun x y -> Sub (x, y))
  let op_mul = token "*" >> return (fun x y -> Mul (x, y))
  let op_div = token "/" >> return (fun x y -> Div (x, y))
  let op_mod = token "%" >> return (fun x y -> Mod (x, y))
  let op_or = token "||" >> return (fun x y -> Or (x, y))
  let op_and = token "&&" >> return (fun x y -> And (x, y))
  let op_ls = token "<" >> return (fun x y -> Less (x, y))
  let op_mr = token ">" >> return (fun x y -> More (x, y))
  let op_lseq = token "<=" >> return (fun x y -> LessEqual (x, y))
  let op_mreq = token ">=" >> return (fun x y -> MoreEqual (x, y))
  let op_eq = token "==" >> return (fun x y -> Equal (x, y))
  let op_neq = token "!=" >> return (fun x y -> NonEqual (x, y))
  let null = token "null" >> return Null

  let ident_obj =
    spaces >> letter <~> many alpha_num => implode >>= function
    | x when List.mem x reserved -> mzero
    | x -> return x

  let parse_string =
    (* char array *)
    let string_of_chars chars =
      (* to string *)
      let buf = Buffer.create 20 in
      List.iter (Buffer.add_char buf) chars;
      Buffer.contents buf
    in
    token "\"" >> many (satisfy (fun x -> x <> '\"')) >>= fun list ->
    token "\"" >> return (ConstExpr (ValString (string_of_chars list)))

  let rec expr input = num_expr input
  and num_expr input = (chainl1 and_expr op_or) input
  and and_expr input = (chainl1 comp_expr op_and) input

  and comp_expr input =
    (chainl1 add_expr
       (op_lseq <|> op_mreq <|> op_ls <|> op_mr <|> op_eq <|> op_neq))
      input

  and add_expr input = (chainl1 mul_expr (op_add <|> op_sub)) input
  and mul_expr input = (chainl1 unar_expr (op_mul <|> op_div <|> op_mod)) input

  and unar_expr input =
    choice
      [
        (token "!" >> lexeme primar_expr >>= fun x -> return (Not x));
        (token "await" >> lexeme primar_expr >>= fun x -> return (Await x));
        ( token "-" >> lexeme primar_expr >>= fun x ->
          return (Sub (ConstExpr (ValInt 0), x)) );
        (token "++" >> lexeme primar_expr >>= fun x -> return (PreInc x));
        (token "--" >> lexeme primar_expr >>= fun x -> return (PreDec x));
        (lexeme primar_expr >>= fun x -> token "++" >> return (PostInc x));
        (lexeme primar_expr >>= fun x -> token "--" >> return (PostDec x));
        primar_expr;
      ]
      input

  and primar_expr input =
    (assign <|> func_call <|> parens expr <|> atomic) input

  and sep_by_coma input = sep_by expr (token ",") input

  and func_call input =
    ( ident_obj >>= fun name ->
      token "(" >> sep_by_coma >>= fun args_list ->
      token ")" >> return (FuncCall (name, args_list)) )
      input

  and assign input =
    (* Check! *)
    let parse_left =
      (*get_field_access <|>*) func_call <|> get_array_variable <|> get_variable
    in
    ( parse_left >>= fun left ->
      token "=" >> expr >>= fun right -> return (Assign (left, right)) )
      input

  and get_variable input = (ident_obj => fun x -> Var x) input

  and get_array_variable input =
    ( ident_obj >>= fun x ->
      ar_braces expr => fun ex -> ArrayAccess (Var x, ex) )
      input

  and atomic input =
    (get_array_variable <|> get_variable
    <|> (convert_to_int >>= fun n -> return (ConstExpr (ValInt n)))
    <|> parse_string
    <|> (token "false" >> return (ConstExpr (ValBool false)))
        (* unite with basic functions/parse_string?? *)
    <|> (token "true" >> return (ConstExpr (ValBool true)))
    <|> null)
      input

  and def_type input =
    choice
      [
        token "int" >> return TypeInt;
        token "string" >> return String;
        token "void" >> return TypeVoid;
        token "bool" >> return TypeBool;
        (ident_obj >>= fun class_id -> return (TypeClass class_id));
      ]
      input
end

(** Statements **)

module Stmt = struct
  open Expr

  let rec parse_stmts input =
    choice
      [
        parse_continue;
        parse_break;
        parse_expr;
        parse_return;
        parse_if;
        parse_while;
        parse_var;
        parse_for;
        parse_stmts_block;
        parse_print;
      ]
      input

  and parse_continue input =
    (token "continue" >> token ";" >> return Continue) input

  and parse_break input = (token "break" >> token ";" >> return Break) input

  and parse_expr input =
    (expr >>= fun express -> token ";" >> return (Expr express)) input

  and parse_return input =
    (token "return"
    >> choice
         [
           ( skip_many1 space >> expr >>= fun result ->
             token ";" >> return (Return (Some result)) );
           token ";" >> return (Return None);
         ])
      input

  and parse_if input =
    ( token "if" >> parens expr >>= fun condition ->
      parse_stmts >>= fun if_body ->
      choice
        [
          ( token "else" >> parse_stmts >>= fun else_body ->
            return (If (condition, if_body, Some else_body)) );
          return (If (condition, if_body, None));
        ] )
      input

  and parse_while input =
    ( token "while" >> parens expr >>= fun condition ->
      parse_stmts >>= fun while_body -> return (While (condition, while_body))
    )
      input

  and parse_var input =
    let helper =
      ident_obj >>= fun var_name ->
      token "=" >> expr
      >>= (fun var_value -> return (var_name, Some var_value))
      <|> return (var_name, None)
    in
    choice
      [
        ( const >>= fun const ->
          def_type >>= fun var_type ->
          sep_by1 helper (token ",") >>= fun var_pair ->
          token ";" >> return (VarDeclr (Some const, var_type, var_pair)) );
        ( def_type >>= fun var_type ->
          sep_by1 helper (token ",") >>= fun var_pair ->
          token ";" >> return (VarDeclr (None, var_type, var_pair)) );
        ( def_type (* array var decl with dimension = 0 *) >>= fun var_type ->
          ident_obj >>= fun one_var ->
          token "[" >> token "]" >> token ";"
          >> return (VarDeclr (None, Array (var_type, 0), [ (one_var, None) ]))
        );
        ( def_type (* array var decl with dimension <> 0 *) >>= fun var_type ->
          ident_obj >>= fun one_var ->
          token "[" >> convert_to_int >>= fun dim ->
          token "]" >> token ";"
          >> return
               (VarDeclr (None, Array (var_type, dim), [ (one_var, None) ])) );
      ]
      input

  and parse_for input =
    ( token "for" >> token "("
    >> choice
         [
           (parse_var >>= fun var -> return (Some var));
           token ";" >> return None;
         ]
    >>= fun declare ->
      choice
        [
          (expr >>= fun expr -> token ";" >> return (Some expr));
          token ";" >> return None;
        ]
      >>= fun condition ->
      sep_by expr (token ",") >>= fun after ->
      token ")" >> parse_stmts >>= fun body ->
      return (For (declare, condition, after, body)) )
      input

  and parse_stmts_block input =
    ( braces (sep_by parse_stmts spaces) >>= fun stmts ->
      return (StmtsBlock stmts) )
      input

  and parse_print input =
    ( token "Console.WriteLine(" >> expr >>= fun expr ->
      token ");" >> return (Print expr) )
      input
end

(** Class functions **)

let get_modifiers =
  many
    (choice
       [
         token "public" >> return Public;
         token "static" >> return Static;
         token "const" >> return Const;
         token "async" >> return Async;
       ])

let get_params =
  Expr.def_type >>= fun p_type ->
  Expr.ident_obj >>= fun name -> return (p_type, name)

let parse_field =
  let helper =
    Expr.ident_obj >>= fun name ->
    token "=" >> Expr.expr
    >>= (fun value -> return (name, Some value))
    <|> return (name, None)
  in
  Expr.def_type >>= fun f_type ->
  sep_by helper (token ",") >>= fun vars ->
  token ";" >> return (VarField (f_type, vars))

let parse_method mdfrs =
  Expr.def_type >>= fun m_type ->
  Expr.ident_obj >>= fun name ->
  token "(" >> sep_by get_params (token ",") >>= fun params ->
  token ")" >> Stmt.parse_stmts >>= fun body ->
  return (Method (mdfrs, m_type, name, params, body))

let parse_class_elements =
  get_modifiers >>= fun mdfrs ->
  parse_field <|> parse_method mdfrs >>= fun class_elems ->
  return (mdfrs, class_elems)

let parse_class =
  get_modifiers >>= fun mdfrs ->
  token "class" >> Expr.ident_obj >>= fun name ->
  token "{" >> sep_by parse_class_elements spaces >>= fun elements ->
  token "}" >> return (Class (mdfrs, name, None, elements))

(** Main function **)

let parser = many parse_class (* Delete many! *)
let apply_parser parser input = parse parser (LazyStream.of_string input)
