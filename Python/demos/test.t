Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

  $ ./demoFactorial.exe
  [1, 2, 6, 24, 120]

  $ ./demoSimpleOOP.exe
  [5, 10]

  $ ./demoWhile.exe
  10

  $ ./demo.exe <<-EOF
  > ./pythonSamples/fact.py
  [1, 2, 6, 24, 120]
