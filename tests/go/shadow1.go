package main
import "fmt"

func main() {
	n := 0;
	{
		n := 1;
		if n == 1 {
			fmt.Print("a");
		}
	}
	if n == 0 {
		fmt.Print("b");
	}
	fmt.Print("\n");
}
