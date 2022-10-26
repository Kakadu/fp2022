(** Copyright 2021-2022, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Csharp_lib.Interpreter_tests

let s =
  "class Program {\n\
  \    static int Primes(int a, int b) {\n\
  \        Console.WriteLine(\"Running primes on [a;b]=\"); \
   Console.WriteLine(a);\n\
  \        Console.WriteLine(b);\n\
  \        Console.WriteLine(\"->\");\n\n\
  \        int count = 0;\n\
  \        int isprime = 1;\n\
  \        int primes[42];\n\n\
  \        for (int i = a; i <= b; i++) {\n\
  \            isprime = 1;\n\n\
  \            for (int c = 2; c <= i / 2; c++) \n\
  \               if (i % c == 0) {\n\
  \                   isprime = 0;\n\
  \               }\n\
  \            if (isprime == 1) {\n\
  \                    Console.WriteLine(i);\n\
  \                    count++;\n\
  \            }\n\
  \        }\n\n\
  \        Console.WriteLine(\"arr test\");\n\n\
  \        for (int i = 0; i < 10; i++) {\n\
  \            primes[i++] = count;\n\
  \        }\n\n\
  \        Console.WriteLine(primes[0]);\n\
  \        Console.WriteLine(primes[1]);\n\
  \    \n\
  \        return count;\n\
  \     }\n\n\
  \     static async void Go() {\n\
  \        int x = await(Primes(2, 10))  ;\n\
  \        Console.WriteLine(\"NPrimes=\"); \n\n\
  \        Console.WriteLine(x); \n\
  \    }\n\n\n\
  \    static void Main() { Console.WriteLine(\"main()\"); Go();\n\
  \       \n\
  \    } }"
(*Stdio.In_channel.input_all stdin*)

(*        int x = await (Test(1, 100) + 1 + await (await (2) + 3) + await (4 + 5));  *)
let () = interpret s false
