Copyright 2021-2022, Artur Gagin
SPDX-License-Identifier: CC0-1.0

Grammar with no %% test.
  $ ./demoParse.exe <<-EOF
  > demo_inputs/demo_input1.mly
  Fatal error: exception Menhir_lib.Interpret.NoSeparator("There is no separator in text (make sure you don't forget symbols %%)")
  Raised at Menhir_lib__Interpret.end_position_of_mly_tokens in file "lib/interpret.ml", line 15, characters 4-101
  Called from Menhir_lib__Interpret.file_text_where_only_tokens_names in file "lib/interpret.ml", line 21, characters 47-80
  Called from Menhir_lib__Interpret.parse' in file "lib/interpret.ml", line 127, characters 32-72
  Called from Dune__exe__DemoParse in file "demos/demoParse.ml", line 24, characters 36-68
  [2]
Grammar with bad token (term) name test.
Expected InvalidToken("1", "$*!JM#QS") where first arg of tuple is line number and last 
one is bad substring.
Note that in REPL.ml we catching this error and getting ParseProcess with next string: 
"Lexer Error: line 1 at: $*!JM#QS".
  $ ./demoParse.exe <<-EOF
  > demo_inputs/demo_input2.mly
  Fatal error: exception Menhir_lib.Lexer.InvalidToken("1", "$*!JM#QS")
  Raised at Menhir_lib__Lexer.tokenizer in file "lib/lexer.ml", line 64, characters 4-50
  Called from Menhir_lib__Lexer.provider in file "lib/lexer.ml", line 69, characters 14-38
  Called from MenhirLib.Convert.traditional2revised.lexer in file "lib/pack/menhirLib.ml", line 152, characters 27-34
  Called from Menhir_lib__Parser._menhir_run_15 in file "lib/parser.ml", line 414, characters 17-45
  Called from Menhir_lib__Interpret.parse' in file "lib/interpret.ml", line 127, characters 4-72
  Called from Dune__exe__DemoParse in file "demos/demoParse.ml", line 24, characters 36-68
  [2]
Grammar with bad nonterm name.
Expected InvalidToken("5", "#*#@DJSLAPr").
In REPL.ml we catching this error and getting ParseProcess with next string: 
"Lexer Error: line 5 at: #*#@DJSLAA#*a".
  $ ./demoParse.exe <<-EOF
  > demo_inputs/demo_input3.mly
  Fatal error: exception Menhir_lib.Lexer.InvalidToken("5", "#*#@DJSLAA#*a")
  Raised at Menhir_lib__Lexer.tokenizer in file "lib/lexer.ml", line 64, characters 4-50
  Called from Menhir_lib__Lexer.provider in file "lib/lexer.ml", line 69, characters 14-38
  Called from MenhirLib.Convert.traditional2revised.lexer in file "lib/pack/menhirLib.ml", line 152, characters 27-34
  Called from Menhir_lib__Parser._menhir_run_04 in file "lib/parser.ml", line 284, characters 17-45
  Called from Menhir_lib__Interpret.parse' in file "lib/interpret.ml", line 130, characters 16-61
  Called from Dune__exe__DemoParse in file "demos/demoParse.ml", line 24, characters 36-68
  [2]
Сorrect grammar test.
  $ ./demoParse.exe <<-EOF
  > demo_inputs/demo_input4.mly
  List of tokens: X Y Z 
  Start rule: main
  Grammar:
  main: X; Y; Z; 
  main: X; Y; 
  main: X; 
