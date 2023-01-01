$ ./demoSingle.exe <<- EOF
> let rec fact n = if n = 0 then 1 else n * fact (n - 1) ;;
> fact 5 ;;
> EOF
val fact : <type_undef> = <fun>
- : <type_undef> = 120

$ ./demoSingle.exe <<- EOF
> let add x y = x + y ;;              
> let add4 = add 4 ;;     
> add4 5 ;;
> EOF
val add : <type_undef> = <fun>
val add4 : <type_undef> = <fun>
- : <type_undef> = 9

$ ./demoSingle.exe <<- EOF
> #help
> EOF
Usage:
 #help         Prints a list of all available commands
 #quit         Exit the toplevel loop and terminate this program
 #use <file>   Read and evaluate source phrases from the given file
