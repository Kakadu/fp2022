Factorial
  $ ./demoSingle.exe << EOF
  > let rec fact n = if n = 0 then 1 else n * fact (n - 1) ;;
  > fact 5 ;;
  > EOF
  val fact : <type_undef> = <fun>
  - : <type_undef> = 120

Partial application
  $ ./demoSingle.exe << EOF
  > let add x y = x + y ;;              
  > let add4 = add 4 ;;     
  > add4 5 ;;
  > EOF
  val add : <type_undef> = <fun>
  val add4 : <type_undef> = <fun>
  - : <type_undef> = 9

Recursive function with multiple arguments
  $ ./demoSingle.exe << EOF
  > let rec pow x y = if y = 0 then 1 else x * pow x (y - 1) ;;
  > pow 4 5 ;;
  > EOF
  val pow : <type_undef> = <fun>
  - : <type_undef> = 1024

Labeled arguments
  $ ./demoSingle.exe << EOF
  > let f ~name2:x ~name1:y = x / y ;;
  > f ~name2:4 ~name1:5 ;;
  > EOF
  val f : <type_undef> = <fun>
  - : <type_undef> = 0

Optional arguments
  $ ./demoSingle.exe << EOF
  > let f ?x:(x = 0) ?y:(y = 0) () ?z:(z = 0) () = x + y + z ;;
  > f ~x:5 () () ;;
  > EOF
  val f : <type_undef> = <fun>
  - : <type_undef> = 5

Use of REPL commands
  $ ./demoSingle.exe << EOF
  > #help
  > EOF
  Usage:
   #help         Prints a list of all available commands
   #quit         Exit the toplevel loop and terminate this program
   #use <file>   Read and evaluate source phrases from the given file
