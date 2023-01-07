func f(x int) {
	if x > 0 {
		g(x - 1);
	} else {
		print(".");
	}
} 

func g(x int) {
	if x % 2 == 0 {
		f(x);
		f(x);
	} else {
		f(x);
	}
}

func main() {
	f(5);
}
