var size = 100;

func range(out chan int) {
	var i = 0;
	for i < size {
		out <- i;
		i = i + 1;
	}
}

func sum(in chan int, out chan int) {
	var i = 0;
	var res = 0;
	for i < 2 * size {
		res = res + (<- in);
		i = i + 1;
	}
	out <- res;
}

func main() {
	var supply chan int;
	var out chan int;

	go range(supply);
	go range(supply);
	go sum(supply, out);

	print(<- out);
}
