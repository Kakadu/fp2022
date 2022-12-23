func filter(arr []int, f func(x int) bool) []int {
	var res = []int {};
	
	var i = 0;
	for i < len(arr) {
		if f(arr[i]) {
			res = append(res, arr[i]);
		}
		i = i + 1;
	}

	return res;
}

func main() {
	var arr = []int {1, 2, 3, 4, 5, 6, 7};
	var arr2 = filter(arr, func(x int) bool {
		return x % 2 == 0;
	});
	print(arr);
	print(arr2);
}
