### An implementaion of OCaml+ADT(AlgebraicDataTypes) mini-language

This is a homework for functional programming course.

License: LGPL for implementation code + WTFPL for test examles in miniLanguage

Author: Emir Vildanov, emirvildanow@gmail.com

Details:
- In order to run REPL.ml run `dune exec ./REPL.exe`
- .t (e.g. `repl.t`) files are tests:
  - $ = command that user execute
  - below = output that appliation 
- dune
  - name -- name, under which library is exported into source files
  - public_name -- name, under which library is exported into other executables within dune file
  - previously was named "jbuilder". In case you see any problems with this name

Requirements:
- OCaml
  - Angstrom (Parser generator <opam install angstrom>)
  - Qcheck (Property-based testing <opam install qcheck>)
  - PpxExpect [ExpectTest] (Another test framework <opam install ppx_expect>)
  - PpxDeriving (TypeDriven code generation <opam install ppx_deriving>)
  - PpxShow (Reimplementation the show plugin from ppx_deriving <opam install ppx_show>)
    - Questionable (Needed in CSharpOOP)
- General
  - Rlwrap (Convinient REPL wrap on terminal <sudo apt-get install rlwrap>)

Features done (append only):

- NOTHINGYET

Features in progress (and TODOs):

- INPROGRESS: Parser
- TODO: Interpreter of non-recursive functions
- TODO: Interpreter of recursive functions is not yet ready
- TODO: make pretty-printing less memory consuming
- TODO: стандартный мини-язык, базовые типы
- TODO: mini-ML с функциями обычными и анонимными, замыканиями и рекурсией
- TODO: алгебраические типы как основной способ проектирования типов
  - в OCaml и Haskell типы int и float -- примитивные (встроенные)
  - тип списков алгебраический и там, и там; в AST не должно быть, что списки отдельно, а алгебраических значения отдельно
  - в OCaml тип bool примитивный, а в Haskell -- алгебраический
- можно поддержать пары, но можно и обойтись алгебраическим типом с одним конструктором
- разумеется, объявления типов, паттерн-мэтчинг и типизация
- присваивание не надо
- исключения не надо


##### Замечания по стилю кодирования

- Если merge request не проходит CI -- проверяться не будет
- Замечания должны быть откомментированы, иначе проверяться не будет.
  - Если исправлены, должны быть поменчены как "исправлены"
  - Если непонятны/некорректны, то это должно быть откомментировано соответствующим образом.

  Такие суровые ограничения вводятся, чтобы замечания не игнорировались.

- Иимена типов и функций -- snake_case
- Имена типов модулей и модулей -- CamelCase
- Ворнинги должны быть пофикшены
- Не стесняйтесь писать `if ... then ... else` вместо `match ... with true -> .. | false -> ...`
- Не стесняйтесь писать гварды в мэтчинге, например
```ocaml
match ... with
| x when f x -> ...
| x          -> ...
| ...
```
вместо
```ocaml
match ... with
| x -> if f x then ... else ...
| ...
```
- Вместо `fun x y -> match y with` лучше писать короче: `fun x -> function`
- Используйте quoted string literals в тестах, чтобы не экранировать руками
```
─( 11:21:01 )─< command 1 >────────────────────────────
utop # {|
  int main () {
    return 0;
  }
  |};;
- : string = "\n  int main () {\n    return 0;\n  }\n  "
```
- Не надо писать
```ocaml
match ... with
| x ->
    Hashtbl.replace tbl key value |> fun () -> ...
```
Лучше
```ocaml
match ... with
| x ->
    let () = Hashtbl.replace tbl key value in
    ...
```
или
```ocaml
match ... with
| x -> (
    Hashtbl.replace tbl key value;
    ...
  )
```
или даже
```ocaml
match ... with
| x -> begin
    Hashtbl.replace tbl key value;
    ...
  end
```
- Не надо писать
```ocaml
let x = if long_expression then true else false in ...
```
лучше
```ocaml
let x = long_expression in ...
```

- 1
