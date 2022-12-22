func forloop(from int, to int, f func(i int)) {
	if from < to {
		f(from);
		forloop(from + 1, to, f);
	}
}

func isprime(n int) bool {
	var found = false;
	
	forloop(2, n, func(i int) {
		found = found || (n % i == 0);
	});
	return !found;
}

func print_string(s string) {
	print(s);
}

func main() {
	forloop(2, 100, func(i int) {
		if isprime(i) {
			print(i, " ");
		}
	});
	print_string("Finished");
}
