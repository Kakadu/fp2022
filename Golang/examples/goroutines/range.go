func range(size int, out chan int) {
	var i = 0;
	for i < size {
		out <- i;
		i = i + 1;
	}
}

func printer(size int, in chan int) {
	var i = 0;
	for i < size {
		print(<- in);
		i = i + 1;
	}
}

func main() {
	var c chan int;
	go range(10, c);
	printer(10, c);
}