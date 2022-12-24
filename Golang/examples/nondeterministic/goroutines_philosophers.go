func philosopher(id int, left chan bool, right chan bool, quit chan bool) {
	print("Philosopher", id, "is hungry", "\n");
	<- left;
	<- right;
	print("Philosopher", id, "is eating", "\n");
	left <- true;
	right <- true;
	print("Philosopher", id, "put down forks", "\n");
	quit <- true;
}

var size = 5;

func makechans(init bool) []chan bool {
	var res []chan bool;
	
	var i = 0;
	for i < size {
		var c chan bool;
		if init {
			c <- true;
		}
		res = append(res, c);
		i = i + 1;
	}

	return res;
}

func main() {
	var forks = makechans(true);
	var quits = makechans(false);

	var i = 0;
	for i < size - 1 {
		go philosopher(i, forks[i], forks[i + 1], quits[i]);
		i = i + 1;
	}
	go philosopher(4, forks[0], forks[4], quits[4]);

	i = 0;
	for i < size {
		print("Waiting for philosopher", i, "\n");
		<- quits[i];
		i = i + 1;
	}
}