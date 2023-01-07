make clean >> o1
make >> o1
touch primes.h
make >> o1

~/University/ocaml/fp2022/Make/_build/default/demo/demoInterpret.exe clean >> o2
~/University/ocaml/fp2022/Make/_build/default/demo/demoInterpret.exe >> o2
touch primes.h
~/University/ocaml/fp2022/Make/_build/default/demo/demoInterpret.exe >> o2

diff o1 o2

