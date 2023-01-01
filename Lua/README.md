### An implementaion of Lua mini-language

This is a homework for functional programming course.

License: LGPL for implementation code + WTFPL for test examles in miniLanguage

Author: Arthur Alekseev, arthur100500@gmail.com

Features done (append only):

- Ast
- Parser
  - with ignoring comments
- Interpreter
- Expressions:
  - mathematics and logic (binary and unary operators)
  - constants
  - functions as constants to pass them as arguments
  - function calls with recursion
  - table init
  - table indexing (with "[]" and ".")
  - variables
- Statements
  - assign (both global and local)
  - function call
  - if elseif else
  - for loop (with counter)
  - while and repeat 
  - do block
  - break and return
  - function declaration
  - standalone expression (for REPL)
 
Features in progress (and TODOs):

- For in loop
- Parse escape chars in strings
- idk, but sure there are plenty

