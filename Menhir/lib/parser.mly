%{ open Ast %}

// are needed
%token TOKEN // %token

%token START // %start

%token RULE
%token RULECOMPONENT
%token EOF

// dont needed (MAYBE?)
//%token LT // <
//%token GT // >
//%token LCBRACE // {
//%token RCBRACE // }
//%token EQUAL // =
//%token LPAREN // ( 
//%token RPAREN // )
//%token DOT // .
//%token EOF

%start main

%% 

main: 
  | EOF
  | TOKEN { TOKENNAME }
  | START { rule
  | PROCENT; PROCENT {rulesread}

rulesread:
  | RULE