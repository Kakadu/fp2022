go: go.o primes.o
	@echo "linking.."
	gcc -Wall -o go go.o primes.o
	@echo "Done"

go.o: go.c primes.h
	@echo "compiling.."
	gcc -Wall -c -o go.o go.c

primes.o: primes.c primes.h
	@echo "compiling.."
	gcc -Wall -c -o primes.o primes.c

clean:
	rm go go.o primes.o
