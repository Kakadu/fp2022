func retarr() []int {
	return []int {10};
}

func changearr(arr []int) {
	arr[0] = 101;
	print(arr);
}

func main() {
	var simplearr = [] int{0,};
	simplearr[0] = 42;
	print(simplearr);

	var arr2d = [][]int { []int {1}, []int {15} };
	arr2d[1][0] = 337;
	print(arr2d);

	retarr()[0] = 15;
	print(retarr());

	var byvalue = []int {0};
	changearr(byvalue);
	print(byvalue);
}
