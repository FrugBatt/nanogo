package main
import "fmt"

func fact(n int) int {
	if n <= 1 {
		return 1;
	}
	return n * fact(n-1);
}

func foo(x int) (int, int) {
	return x, x+1
}

func main() {
	x,y := foo(fact(20))
	fmt.Print(x+y+1, "\n");
}
