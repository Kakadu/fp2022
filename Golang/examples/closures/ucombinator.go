func Y(g func(f func() func(x int) int) func(x int) int) func(x int) int {
	return g(func() func(x int) int {
		return Y(g);
	});
}

func main() {

	var fact_nonrec = func(f func() func(x int) int) func(x int) int {
		return func(x int) int {
			if x == 0 {
				return 1;
			} else {
				return x * f()(x - 1);
			}
		};
	};

	var fact = Y(fact_nonrec);

	print(fact(1), fact(2), fact(5), fact(10));
}
