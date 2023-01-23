package main
import "fmt"

func main() {
	x := 1
	p := &x
	fmt.Print(*p)
	fmt.Print("\n")
	*p = 2
	fmt.Print(*p, "\n")
	fmt.Print(p,"\n")
}

