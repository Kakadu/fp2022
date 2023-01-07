
  $ ./INTERPRET.exe examples/func/id.go
  10
  $ ./INTERPRET.exe examples/func/fact_rec.go
  1 2 120 3628800
  $ ./INTERPRET.exe examples/func/fibonacci_rec.go
  0 2 55 610 4181
  $ ./INTERPRET.exe examples/func/rec_same_var.go
  012345678910
  $ ./INTERPRET.exe examples/func/hello_world.go
  10 Hello, World!
  9 Hello, World!
  8 Hello, World!
  7 Hello, World!
  6 Hello, World!
  5 Hello, World!
  4 Hello, World!
  3 Hello, World!
  2 Hello, World!
  1 Hello, World!
  $ ./INTERPRET.exe examples/arrays/arr_assign.go
  [42][[1], [337]][10][101][0]
  $ ./INTERPRET.exe examples/arrays/array_map.go
  [1, 2, 3, 4, 5, 6, 7][1, 8, 27, 64, 125, 216, 343]
  $ ./INTERPRET.exe examples/arrays/array_filter.go
  [1, 2, 3, 4, 5, 6, 7][2, 4, 6]
  $ ./INTERPRET.exe examples/global_var_order.go
  1
  $ ./INTERPRET.exe examples/for_if/primes_for.go
  2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 
  $ ./INTERPRET.exe examples/var_default.go
  0false[]chan
  $ ./INTERPRET.exe examples/goroutines/single_message.go
  yes
  $ ./INTERPRET.exe examples/goroutines/range.go
  0123456789
  $ ./INTERPRET.exe examples/goroutines/sum_of_two_ranges.go
  9900
  $ ./INTERPRET.exe examples/func/mutually_recursive.go
  ........
  $ ./INTERPRET.exe examples/closures/curry.go
  3 11 
  20 40
  $ ./INTERPRET.exe examples/closures/ucombinator.go
  1 2 120 3628800
