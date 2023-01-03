### An implementaion of Prolog mini-language

This is a homework for functional programming course.

License: LGPL for implementation code + WTFPL for test examles in miniLanguage

Author: Ilya Shchuckin, theilia509@gmail.com

#### Description:

Even though this is a mini-language interpreter it still tries it's best to meet **ISO 13211-1:1995** which is a standart for prolog language. 

REPL was heavily inspired by **swi-prolog**. The idea was to make it possible to automatically compare the outputs of the two. Unfortunately, it doesn't work well on complex examples as **swi-prolog** does a lot of sophisicated output transformations.

#### Features done:
- Basic parser  
- Herbrand algorithm
- Backtracking
- Support of some builtin predicates:
  - ```true/0```
  - ```(==)/2```
  - ```(,)/2```
  - ```(!)/0```
  - ```read/1```
  - ```writeln/1``` (Non ISO)
  - ```clause/2```
- Simple REPL

##### Repl
  ```
  $ ../REPL.exe ../demos/examples/Test7.pl <<"EOF"
  ```
  ```Prolog
  > eliza([i, love, you], Response).
  Response = [why, do, you, love, me, "?"].
  ```
  REPL takes a path to prolog text as an argument and waits for user to make a query.


  ```
  $ ../REPL.exe ../demos/examples/Test6.pl <<"EOF"
  ```
  ```Prolog
  > clause(insect(I), T).
  
  I = ant,
  T = true ;
  > N
  I = bee,
  T = true.
  ```

  Interpreter will not try to search for every satisfying substituion at once. Instead it will provide (if possible) only one and wait for user to type ```N``` to start backtracking.

#### Tests and examples:

- Parser tests can be found in ```lib/tests.ml```
- Interpeter tests are located in ```demos/```
  - ```repl.t``` contains test descriptions (prolog text path, input)
  - ```examples/``` contains prolog texts that can be converted to databases and used for testing


#### Documentation: 
This project has a quiet detailed documentation which can be generated and accessed by odoc.
