package main
import "fmt"
type A struct {
	a int
}
func main() {
	var a A 
	a.a = 2
	_ = a.a
  fmt.Print(a);
}
