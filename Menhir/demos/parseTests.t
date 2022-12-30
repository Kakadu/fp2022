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
  $ ./demoParse.exe <<-EOF
  > demo_inputs/demo_input1.mly
  List of tokens: PLUS 
  Start rule: main
  Grammar:
  main: PLUS; 
