func forloop(from int, to int, f func(i int)) {
	if from < to {
		f(from);
		forloop(from + 1, to, f);
	}
}

func main() {
	var sum = 0;

	forloop(0, 25, func(i int) {
		sum = sum + i * i;
	});

	print(sum);
}