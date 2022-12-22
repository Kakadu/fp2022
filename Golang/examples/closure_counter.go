func counter() func() int {
	var i = -1;
	var f = func() int {
		i = i + 1;
		return i;
	};
	return f;
}

func main() {
	var next = counter();
	print(next());
	print(next());
	print(next());
	print(next());
	print(next());
	print(next());
	print(next());
	print(next());
}