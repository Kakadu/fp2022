func yes(c chan string) {
	c <- "yes";
}

func main() {
	var c chan string;
	go yes(c);
	print(<- c);
}