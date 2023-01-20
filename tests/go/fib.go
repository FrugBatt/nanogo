package main
import "fmt"

func fib(n int) int {
	a, b := 0, 1
	for ; n > 0; n-- {
		a, b = b, a+b
	}
	return a;
}

func main() {
  for n := 0; n <= 10; n++ {
	  fmt.Print(fib(n));
	  fmt.Print("\n")
  }
}
