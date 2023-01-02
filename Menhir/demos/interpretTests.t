Copyright 2021-2022, Artur Gagin
SPDX-License-Identifier: CC0-1.0

Correct grammar tests.
  $ ./demoInterpret.exe <<-EOF
  > demo_inputs/demo_input.mly
  > LBRACE INT RBRACE EOL
  The file was successfully parsed.
   [ main :  [ expr :  LBRACE   [ expr :  INT  ]   RBRACE  ]   EOL  ] 
  $ ./demoInterpret.exe <<-EOF
  > demo_inputs/demo_input.mly
  > LBRACE INT MULTY INT PLUS LBRACE INT DIV INT RBRACE RBRACE EOL
  The file was successfully parsed.
   [ main :  [ expr :  LBRACE   [ expr :  INT   [ expr' :  MULTY   [ expr :  INT  ]   [ expr' :  PLUS   [ expr :  LBRACE   [ expr :  INT   [ expr' :  DIV   [ expr :  INT  ]  ]  ]   RBRACE  ]  ]  ]  ]   RBRACE  ]   EOL  ] 
  $ ./demoInterpret.exe <<-EOF
  > demo_inputs/demo_input.mly
  > LBRACE
  The file was successfully parsed.
  OVERSHOOT
  $ ./demoInterpret.exe <<-EOF
  > demo_inputs/demo_input.mly
  > INT EOL
  The file was successfully parsed.
   [ main :  [ expr :  INT  ]   EOL  ] 
  $ ./demoInterpret.exe <<-EOF
  > demo_inputs/demo_input.mly
  > INT DIV INT PLUS INT MULTY INT EOL
  The file was successfully parsed.
   [ main :  [ expr :  INT   [ expr' :  DIV   [ expr :  INT  ]   [ expr' :  PLUS   [ expr :  INT  ]   [ expr' :  MULTY   [ expr :  INT  ]  ]  ]  ]  ]   EOL  ] 
  $ ./demoInterpret.exe <<-EOF
  > demo_inputs/demo_input.mly
  > INT DIV
  The file was successfully parsed.
  OVERSHOOT
  $ ./demoInterpret.exe <<-EOF
  > demo_inputs/demo_input.mly
  > DIV
  The file was successfully parsed.
  REJECT
  $ ./demoInterpret.exe <<-EOF
  > demo_inputs/demo_input.mly
  > RBRACE INT LBRACE EOL
  The file was successfully parsed.
  REJECT
  $ ./demoInterpret.exe <<-EOF
  > demo_inputs/demo_input.mly
  > INT MULTY INT EOL
  The file was successfully parsed.
   [ main :  [ expr :  INT   [ expr' :  MULTY   [ expr :  INT  ]  ]  ]   EOL  ] 
  $ ./demoInterpret.exe <<-EOF
  > demo_inputs/demo_input.mly
  > MULTY INT INT EOL
  The file was successfully parsed.
  REJECT
Test with grammar error.
  $ ./demoInterpret.exe <<-EOF
  > demo_inputs/demo_input1.mly
  > PLUS
  Some error in parse part
  [1]
