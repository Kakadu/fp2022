func retarr() [1]int {
	return [1]int {10};
}

func changearr(arr [1]int) {
	arr[0] = 101;
	print(arr);
}

func main() {
	var simplearr = [1] int{0,};
	simplearr[0] = 42;
	print(simplearr);

	var arr2d = [2][1]int { [1]int {1}, [1]int {15} };
	arr2d[1][0] = 337;
	print(arr2d);

	retarr()[0] = 15;
	print(retarr());

	var byvalue = [1]int {0};
	changearr(byvalue);
	print(byvalue);
}
