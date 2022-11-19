func fact(x int) int {
	if x < 1 {
		return 1;
	} else {
		return x * fact(x-1);
	}
}

func main() {
	print(fact(1), fact(2), fact(5), fact(10));
}