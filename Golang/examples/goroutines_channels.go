func main() {
	var c chan string;
	c <- "yes";
	print(<- c);
}