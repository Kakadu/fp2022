Simple inputs without let
  $ ./demo.exe <<EOF
  > 1+2;;
  > EOF
  3 : int
  $ ./demo.exe <<EOF
  > "hello";;
  > EOF
  hello : string
  $ ./demo.exe <<EOF
  > if true then 1 else 3;;
  > EOF
  1 : int
Inputs with let
  $ ./demo.exe <<EOF
  > let x = 1 in match x with 1 -> true | 2 -> false;;
  > EOF
  true : bool
  $ ./demo.exe <<EOF
  > let rec fact x = match x with 1 -> 1 | x -> x*(fact (x-1)) in fact 5;;
  > EOF
  120 : int
