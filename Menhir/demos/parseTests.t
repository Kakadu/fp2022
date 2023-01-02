Copyright 2021-2022, Artur Gagin
SPDX-License-Identifier: CC0-1.0

Grammar with no %% test.
  $ ./demoParse.exe <<-EOF
  > demo_inputs/demo_input1.mly
  There is no separator in text (make sure you don't forget symbols %%)
  [1]
Grammar with bad token (term) name test.
Expected InvalidToken("1", "$*!JM#QS") where first arg of tuple is line number and last 
one is bad substring.
Note that in REPL.ml we catching this error and getting ParseProcess with next string: 
"Lexer Error: line 1 at: $*!JM#QS".
  $ ./demoParse.exe <<-EOF
  > demo_inputs/demo_input2.mly
  InvalidToken error: line 1 at $*!JM#QS
  [1]
Grammar with bad nonterm name.
Expected InvalidToken("5", "#*#@DJSLAPr").
In REPL.ml we catching this error and getting ParseProcess with next string: 
"Lexer Error: line 5 at: #*#@DJSLAA#*a".
  $ ./demoParse.exe <<-EOF
  > demo_inputs/demo_input3.mly
  InvalidToken error: line 5 at #*#@DJSLAA#*a
  [1]
Сorrect grammar test.
  $ ./demoParse.exe <<-EOF
  > demo_inputs/demo_input4.mly
  List of tokens: X Y Z 
  Start rule: main
  Grammar:
  main: X; Y; Z; 
  main: X; Y; 
  main: X; 
  $ ./demoParse.exe <<-EOF
  > demo_inputs/demo_input.mly
  List of tokens: PLUS MINUS MULTY DIV LBRACE RBRACE INT EOL 
  Start rule: main
  Grammar:
  main: expr; EOL; 
  main: EOL; 
  expr: LBRACE; expr; RBRACE; expr'; 
  expr: INT; expr'; 
  expr: LBRACE; expr; RBRACE; 
  expr: INT; 
  expr': PLUS; expr; expr'; 
  expr': MINUS; expr; expr'; 
  expr': MULTY; expr; expr'; 
  expr': DIV; expr; expr'; 
  expr': PLUS; expr; 
  expr': MINUS; expr; 
  expr': MULTY; expr; 
  expr': DIV; expr; 
