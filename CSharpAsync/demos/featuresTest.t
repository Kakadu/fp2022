Features tests
Test 0 (Primes test - no features)
  $ ./demoShowContext.exe <<-EOF
  > class Program
  > {
  >   static int Primes(int a, int b) {
  >        int count = 0;
  >        int isprime = 1;
  > 
  >       for (int i = a; i <= b; i++) {
  >            isprime = 1;
  >  
  >            for (int c = 2; c <= i / 2; c++) 
  >               if (i % c == 0) {
  >                   isprime = 0;
  >               }
  >            if (isprime == 1) {
  >                   Console.WriteLine(i);
  >                   count++;
  >            }
  >       }
  >        return count;
  >    }
  > 
  >    static void Main()
  >    { 
  >       int x = Primes(2, 10);
  >       Console.WriteLine("NPrimes="); 
  >       Console.WriteLine(x); 
  >    } 
  > }
  > EOF
  2
  3
  5
  7
  NPrimes=
  4
  

Test 1 (Primes test - await)
  $ ./demoShowContext.exe <<-EOF
  > class Program
  > {
  >   static int Primes(int a, int b) {
  >        int count = 0;
  >        int isprime = 1;
  > 
  >       for (int i = a; i <= b; i++) {
  >            isprime = 1;
  >  
  >            for (int c = 2; c <= i / 2; c++) 
  >               if (i % c == 0) {
  >                   isprime = 0;
  >               }
  >            if (isprime == 1) {
  >                   Console.WriteLine(i);
  >                   count++;
  >            }
  >       }
  >        return count;
  >    }
  > 
  >    static void Main()
  >    { 
  >       int x = await (Primes(2, 10));
  >       Console.WriteLine("NPrimes="); 
  >       Console.WriteLine(x); 
  >    } 
  > }
  > EOF
  2
  3
  5
  7
  NPrimes=
  4
  

Test 2 (Multy await)
  $ ./demoShowContext.exe <<-EOF
  > class Program
  > {
  >    static void Main()
  >    { 
  >       int x = await(1 + await(await(2) + 3) + await(4 + 5));
  >       Console.WriteLine(x); 
  >    } 
  > }
  > EOF
  15
  

Test 3 (Await requires async in a method)
  $ ./demoShowContext.exe <<-EOF
  > class Program
  > {
  > 
  >    static void NoAsyncModifier() {
  >       int x = await(1 + await(await(2) + 3) + await(4 + 5));
  >       Console.WriteLine(x);        
  >    }
  >     
  >    static void Main()
  >    { 
  >       NoAsyncModifier();
  >    } 
  > }
  > EOF
  Attempt to run from a non-async context


Test 4 (Await requires async in method - ok)
  $ ./demoShowContext.exe <<-EOF
  > class Program
  > {
  > 
  >    async static void WithAsyncModifier() {
  >       int x = await(1 + await(await(2) + 3) + await(4 + 5));
  >       Console.WriteLine(x);        
  >    }
  >     
  >    static void Main()
  >    { 
  >       WithAsyncModifier();
  >    } 
  > }
  > EOF
  15
  

Test 5 (Int array access)
  $ ./demoShowContext.exe <<-EOF
  > class Program
  > {
  >    static void Main()
  >    { 
  >       int x[42];
  >       x[13] = 13;
  >       x[13] = 56;
  >       x[12] = 14;
  >       Console.WriteLine(x[13]-x[12]); 
  >    } 
  > }
  > EOF
  42
  

Test 6 (String array access)
  $ ./demoShowContext.exe <<-EOF
  > class Program
  > {
  >    static void Main()
  >    { 
  >       string s[42];
  >       s[13] = "oc";
  >       s[12] = "aml";
  >       Console.WriteLine(s[13]+s[12]); 
  >    } 
  > }
  > EOF
  ocaml
  

Test 7 (Wrong array access)
  $ ./demoShowContext.exe <<-EOF
  > class Program
  > {
  >    static void Main()
  >    { 
  >       int s[42];
  >       s[13] = "bug";
  >    } 
  > }
  > EOF
  Wrong array type


Test 8 (Wrong index access)
  $ ./demoShowContext.exe <<-EOF
  > class Program
  > {
  >    static void Main()
  >    { 
  >       int x[42];
  >       Console.WriteLine(x[42]);
  >    } 
  > }
  > EOF
  Index out of bounds


Test 9 (Primes test - arrays)
  $ ./demoShowContext.exe <<-EOF
  > class Program
  > {
  >   static int Primes(int a, int b) {
  >        int count = 0;
  >        int isprime = 1;
  >        int primes[100]; 
  > 
  >       for (int i = a; i <= b; i++) {
  >            isprime = 1;
  >  
  >            for (int c = 2; c <= i / 2; c++) 
  >               if (i % c == 0) {
  >                   isprime = 0;
  >               }
  >            if (isprime == 1) {
  >                   primes[count] = i;
  >                   count++;
  >            }
  >        }
  > 
  >        for (int i = 0; i < count; i++) {
  >             Console.WriteLine(primes[i]);
  >        }     
  >        return count;
  >    }
  > 
  >    static void Main()
  >    { 
  >       int x = Primes(2, 10);
  >       Console.WriteLine("NPrimes="); 
  >       Console.WriteLine(x); 
  >    } 
  > }
  > EOF
  2
  3
  5
  7
  NPrimes=
  4
  

