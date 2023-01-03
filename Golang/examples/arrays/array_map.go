func map(arr []int, f func(x int) int) []int {
	var i = 0;
	for i < len(arr) {
		arr[i] = f(arr[i]);
		i = i + 1;
	}
	return arr;
}

func main() {
	var arr = []int {1, 2, 3, 4, 5, 6, 7};
	var arr2 = map(arr, func(x int) int {
		return x * x * x;
	});
	print(arr);
	print(arr2);
}