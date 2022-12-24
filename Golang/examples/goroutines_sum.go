var size = 100;

func range(out chan int) {
	var i = 0;
	for i < size {
		out <- i;
		i = i + 1;
	}
}

func sum(in chan int, loop chan int, out chan int) {
	var i = 0;
	for i < 2 * size {
		var cur = <- loop;
		cur = cur + (<- in);
		loop <- cur;
		i = i + 1;
	}
	out <- (<- loop);
}

func main() {
	var supply chan int;
	var loop chan int;
	loop <- 0;
	var out chan int;

	go range(supply);
	go range(supply);
	go sum(supply, loop, out);

	print(<- out);
}
