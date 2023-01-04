func forloop(from int, to int, f func(i int)) {
	if from < to {
		f(from);
		forloop(from + 1, to, f);
	}
}

func isprime(n int) bool {
	var i = 2;
	for i < n {
		if n % i == 0 {
			return false;
		}
		i = i + 1;
	}

	return true;
}


func main() {
	var i = 2;
	for i < 100 {
		if isprime(i) {
			print(i);
			print(" ");
		}
		i = i + 1;
	}
}
