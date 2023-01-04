func times(i int, f func(i int)) {
	f(i);
	if i > 1 {
		times(i - 1, f);
	}
}

func helloworld(i int) {
	print(i, "Hello, World!\n");
}

func main() {
	times(10, helloworld);
}
