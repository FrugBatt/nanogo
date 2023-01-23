package main
import "fmt"
func main() {
	fmt.Print("a")
	fmt.Print()
	fmt.Print("b\n")
	fmt.Print(true, "\n")
	fmt.Print(false, "\n")
	fmt.Print(1, 2, 3, "\n")
	fmt.Print(1, "2", 3, "\n")
	fmt.Print(1, "2", 3, 4, "5", "\n")
	fmt.Print(1, "2", 3, true, "5", "\n")
	s := "s"
	fmt.Print(1, s, 3, "\n")
	fmt.Print(nil, "\n")
	var p *int = nil
	fmt.Print("a", p, "b\n")
}
