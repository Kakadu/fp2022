### An implementaion of "OCaml with labeled and optional arguments" mini-language

This is a homework for functional programming course.

License: LGPL for implementation code + WTFPL for test examles in miniLanguage

Author: Denis Porsev, den.porsev@gmail.com

--- 

### Implementation features:
#### Features done:

- Parser
- Interpreter
- Pretty-printing for ast types and errors
- Simple read-eval-print-loop
- Integration tests

#### Features in progress (and TODOs):

- [ ] Type inference with polymorphism is a WIP
- [ ] More thorough testing
- [ ] Documentation
- [ ] Extensions of REPL (new commands, better exception handling)
--- 

### Language features:

#### Supported language features:
- Primitive types: booleans, integers, unit type
- Binary operators: addition, multiplication, subtraction, division and comparisons
- Let expressions and definitions
- Functions: basic, anonymous, recursive, closures; partial application is available
- Labeled and optional arguments

#### Possible language extensions:
- Unary operators, lists, pattern matching, option types
