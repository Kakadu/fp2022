func fibhelper(x int, y int, n int) int {
	if n == 0 {
		return x;
	} else {
		var next_x = y;
		var next_y = x + y;
		return fibhelper(next_x, next_y, n - 1);
	}
}

func fib(n int) int {
	if n < 0 {
		return -1;
	} else {
		return fibhelper(0, 1, n);
	}
}

func main() {
	print(fib(0), fib(3), fib(10), fib(15), fib(19));
}
