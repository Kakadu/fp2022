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
  > let f x = x+3 in f 5;;
  > EOF
  8 : int
  $ ./demo.exe <<EOF
  > let f x y z = (x+y)/z in f 1 2 1;;
  > EOF
  3 : int
  $ ./demo.exe <<EOF
  > let rec f x = f x in 1;;
  > EOF
  1 : int
  $ ./demo.exe <<EOF
  > let x = 3 in (fun y -> y+3) x;;
  > EOF
  6 : int
Factorial
  $ ./demo.exe <<EOF
  > let rec fact x = match x with 1 -> 1 | x -> x*(fact (x-1)) in fact 1;;
  > EOF
  1 : int
  $ ./demo.exe <<EOF
  > let rec fact x = match x with 1 -> 1 | x -> x*(fact (x-1)) in fact 5;;
  > EOF
  120 : int
Fibonacci
  $ ./demo.exe <<EOF
  > let rec fib n = match n with 0 -> 1 | 1 -> 1 | x -> (fib (x-2)+fib (x-1)) in fib 0;;
  > EOF
  1 : int
  $ ./demo.exe <<EOF
  > let rec fib n = match n with 0 -> 1 | 1 -> 1 | x -> (fib (x-2)+fib (x-1)) in fib 2;;
  > EOF
  2 : int
  $ ./demo.exe <<EOF
  > let rec fib n = match n with 0 -> 1 | 1 -> 1 | x -> (fib (x-2)+fib (x-1)) in fib 4;;
  > EOF
  5 : int
