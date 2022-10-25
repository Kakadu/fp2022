open Csharp_lib.Interpreter_tests

let s = "class Program \
{
    static int Primes(int a, int b) {
        Console.WriteLine(\"Running primes on [a;b]=\"); \
        Console.WriteLine(a);
        Console.WriteLine(b);
        Console.WriteLine(\"->\");

        int count = 0;
        int isprime = 1;
        int primes[42];

        for (int i = a; i <= b; i++) {
            isprime = 1;

            for (int c = 2; c <= i / 2; c++) 
               if (i % c == 0) {
                   isprime = 0;
               }
            if (isprime == 1) {
                    Console.WriteLine(i);
                    count++;
            }
        }

        Console.WriteLine(\"arr test\");

        for (int i = 0; i < 10; i++) {
            primes[i++] = count;
        }

        Console.WriteLine(primes[0]);
        Console.WriteLine(primes[1]);
    
        return count;
     }

     static async void Go() {
        int x = await(Primes(2, 10))  ;
        Console.WriteLine(\"NPrimes=\"); 

        Console.WriteLine(x); 
    }


    static void Main() \
    { \
        Console.WriteLine(\"main()\"); \
        Go();
       
    } \
}" (*Stdio.In_channel.input_all stdin*)

(*        int x = await (Test(1, 100) + 1 + await (await (2) + 3) + await (4 + 5));  *)
let () = interpret s false
