(** Copyright 2021-2022, Artur Gagin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

%token <string> TOKEN
%token <string> START
%token PROCENTPROCENT "%%"
%token VERT "|"
%token <string> NONTERM 
%token SEMICOLON ";"
%token <string> RULECOMPONENT 
%token EOF

(* token list * start_rule *)
%start <((string list) * string)> token_and_start
(* grammar *)
%start <((string * string list) list)> grammar
%%

let token_and_start :=
    | t = tokens; s = start; EOF; { t, s }

let tokens :=
    | t = TOKEN; tkns = tokens; { t :: tkns }
    | t = TOKEN; { [t] }

let start :=
    | s = START; { s }

let grammar := 
    | "%%"; ntrms = nonterms; EOF; { ntrms }

let nonterms := 
    | n = NONTERM; rls = rules; nms = nonterms; { (List.map (fun a -> n, a) rls) @ nms }
    | n = NONTERM; rls = rules; { (List.map (fun a -> n, a) rls) }

let rules :=
    | "|"; rc = rulecomps; rls = rules; { rc :: rls }
    | "|"; rc = rulecomps; { [rc] }

let rulecomps :=
    | r = RULECOMPONENT; ";"; rc = rulecomps; { r :: rc }
    | r = RULECOMPONENT; { [r] }
