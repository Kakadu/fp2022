Set variable k to local environment
  $ ./demoInterpret.exe <<-"EOF"
  > k=1 echo hello
  > echo k=$k
  > EOF
  hello
  k=
  Interpretation finished with return code: 0

  $ bash <<-"EOF"
  > k=1 echo hello
  > echo k=$k
  > EOF
  hello
  k=

Set several global variables
  $ ./demoInterpret.exe <<-"EOF"
  > c=27 b=10
  > echo c=$c b=$b
  > EOF
  c=27 b=10
  Interpretation finished with return code: 0

  $ bash <<-"EOF"
  > c=27 b=10
  > echo c=$c b=$b
  > EOF
  c=27 b=10

Set global variables in pipe and use this variables in operand
  $ ./demoInterpret.exe <<-"EOF"
  > a=10 && echo $a | cat
  > echo a=$a
  > EOF
  10
  a=10
  Interpretation finished with return code: 0

  $ bash <<-"EOF"
  > a=10 && echo $a | cat
  > echo a=$a
  > EOF
  10
  a=10

Only variables from the first operand will be exposed
to the global environment
  $ ./demoInterpret.exe <<-"EOF"
  > c=27 || e=28 && b=29 | e=2 && echo some
  > echo c=$c b=$b e=$e
  > EOF
  some
  c=27 b= e=
  Interpretation finished with return code: 0

  $ bash <<-"EOF"
  > c=27 || e=28 && b=29 | e=2 && echo some
  > echo c=$c b=$b e=$e
  some
  c=27 b= e=

Check the correct operation of the logical or:
the return code of any assignment is 0
-> the right side of the boolean operand or will never be executed
  $ ./demoInterpret.exe <<-"EOF"
  > c=27 || e=28 && echo hello | f=2 && echo some
  > echo c=$c e=$e f=$f
  > EOF
  some
  c=27 e= f=
  Interpretation finished with return code: 0

  $ bash <<-"EOF"
  > c=27 || e=28 && echo hello | f=2 && echo some
  > echo c=$c e=$e f=$f
  some
  c=27 e= f=

Сhecking the correct operation of the pipes
  $ ./demoInterpret.exe <<-"EOF"
  > c=27 && echo hello | cat
  > echo c=$c
  > EOF
  hello
  c=27
  Interpretation finished with return code: 0

  $ bash <<-"EOF"
  > c=27 && echo hello | cat
  > echo c=$c
  > EOF
  hello
  c=27

Set several variables to global environment in first operand
  $ ./demoInterpret.exe <<-"EOF"
  > c=27 && d=28 && echo hello | cat
  > echo c=$c d=$d
  > EOF
  hello
  c=27 d=28
  Interpretation finished with return code: 0

  $ bash <<-"EOF"
  > c=27 && d=28 && echo hello | cat
  > echo c=$c d=$d
  > EOF
  hello
  c=27 d=28

Set several variables to global environment in second operand
  $ ./demoInterpret.exe <<-"EOF"
  > b=10 | c=27 && echo hello | cat
  > echo b=$b c=$c
  > EOF
  hello
  b= c=
  Interpretation finished with return code: 0

  $ bash <<-"EOF"
  > b=10 | c=27 && echo hello | cat
  > echo b=$b c=$c
  > EOF
  hello
  b= c=

In this test we are trying to override the value for a variable (e)
  $ ./demoInterpret.exe <<-"EOF"
  > c=27 && e=28 && echo hello | e=2 || d=10
  > echo c=$c e=$e d=$d
  > EOF
  c=27 e=28 d=
  Interpretation finished with return code: 0

  $ bash <<-"EOF"
  > c=27 && e=28 && echo hello | e=2 || d=10
  > echo c=$c e=$e d=$d
  > EOF
  c=27 e=28 d=

Test or operator (||) with variable assignment
  $ ./demoInterpret.exe <<-"EOF"
  > c=27 || e=28 && echo hello | e=2
  > echo c=$c e=$e
  > EOF
  c=27 e=
  Interpretation finished with return code: 0

  $ bash <<-"EOF"
  > c=27 || e=28 && echo hello | e=2
  > echo c=$c e=$e
  > EOF
  c=27 e=

Test or opeator (||) with cmd
  $ ./demoInterpret.exe <<-"EOF"
  > c=27 || e=28 && echo hello | f=2 || echo some
  > echo c=$c e=$e f=$f
  > EOF
  c=27 e= f=
  Interpretation finished with return code: 0

  $ bash <<-"EOF"
  > c=27 || e=28 && echo hello | f=2 || echo some
  > echo c=$c e=$e f=$f
  c=27 e= f=
