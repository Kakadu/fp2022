func add(x int) func(y int) int {
	return func(y int) int {
		return x + y;
	};
}

func main() {
	var inc = add(1);
	print(inc(2), inc(10), "\n");
	var plusTen = add(10);
	print(plusTen(10), add(10)(30));
}