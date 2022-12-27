  $ ./demoInterp.exe <<-EOF
  > function f(x)
  >   if x >= 1 then return f(x - 1) * x end
  >   return 1
  > end
  > print("Factorial")
  > for i = 1, 10 do
  >   print(f(i))
  > end
  > EOF
  Factorial
  1.
  2.
  6.
  24.
  120.
  720.
  5040.
  40320.
  362880.
  3628800.
  $ ./demoInterp.exe <<-EOF
  > f = function(x)
  >   if x < 2 then
  >     return 1
  >   else
  >     return f(x - 2) + f(x - 1)
  >   end
  > end
  > 
  > print()
  > print("Fibonacci")
  > for i = 1, 10 do
  >   print(f(i))
  > end
  > EOF
  Fibonacci
  1.
  2.
  3.
  5.
  8.
  13.
  21.
  34.
  55.
  89.
  $ ./demoInterp.exe <<-EOF
  > a = 4
  > 
  > function b()
  >   a = 3
  >   local a = 5
  >   print("local:")
  >   print(a)
  > end
  > 
  > print()
  > print("Global/local")
  > print("global:")
  > print(a)
  > b()
  > print("global after change:")
  > print(a)
  > EOF
  Global/local
  global:
  4.
  local:
  5.
  global after change:
  3.
  $ ./demoInterp.exe <<-EOF
  > -- Bool to string
  > function b2s(b) if b then return "true" else return "false" end end
  > print("Table tests")
  > a = {}
  > print("Empty table create: " .. b2s(a == {}))
  > a = {1, 2, 3}
  > print("Filled table create: " .. b2s(a[1] == 1 and a[2] == 2 and a[3] == 3))
  > a = {[6] = 5, 2, 3, [0] = "s"}
  > print("Filled indexed table create: " .. b2s(a[1] == 2 and a[2] == 3 and a[0] == "s" and a[6] == 5))
  > a = {["a"] = 4, ["b"] = 9, ["huge_giant_name"] = 42}
  > print("String indexing like fields: " .. b2s(a["a"] == 4 and a.b == 9 and a.huge_giant_name == 42))
  > a = {a = 4, b = 9, huge_giant_name = 42}
  > print("Creating fields in table: " .. b2s(a["a"] == 4 and a.b == 9 and a.huge_giant_name == 42))
  > a = {{{{}, {2}}, {}}, {b = {3}}}
  > print("Inner tables: " .. b2s(a[1][1][2][1] == 2 and a[2].b == {3} and a[2].b[1] == 3))
  > EOF
  Table tests
  Empty table create: true
  Filled table create: true
  Filled indexed table create: true
  String indexing like fields: true
  Creating fields in table: true
  Inner tables: true
  $ ./demoInterp.exe <<-EOF
  > print("Arithmetics and precedence")
  > print(1 + 2 * 3)
  > print(1 * 2 + 3)
  > print(3 + 4 * 9 - 2 ^ 4 * 3)
  > print(3 - 1 - 1)
  > EOF
  Arithmetics and precedence
  7.
  5.
  -9.
  1.
  $ ./demoInterp.exe <<-EOF
  > function map(lst, f)
  >   local i = 1
  >   while lst[i] do
  >     lst[i] = f(lst[i])
  >     i = i + 1
  >   end
  >   return lst
  > end
  > 
  > l = {2, 4, 6, 8, 10, 12, 14, 17}
  > f_ = function(x) return x ^ 2 end
  > l1 = map(l, f_)
  > 
  > print("Map function")
  > for i = 1, 8 do
  >   print(l1[i])
  > end
  > EOF
  Map function
  4.
  16.
  36.
  64.
  100.
  144.
  196.
  289.
