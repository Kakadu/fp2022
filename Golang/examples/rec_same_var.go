func f(x int) {
	var i = x;
	if x > 0 {
		f(x - 1);
	}
	print(i);
}

func main() {
	f(10);
}
