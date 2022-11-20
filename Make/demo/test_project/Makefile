go: go.o primes.o
	gcc -Wall -o go go.o primes.o

go.o: go.c primes.h go.o
	gcc -Wall -c -o go.o go.c

primes.o: primes.c primes.h
	gcc -Wall -c -o primes.o primes.c

clean:
	rm go go.o primes.o
