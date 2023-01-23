package main
import "fmt"
type T struct { a int; b bool; p *int }
func main() {
	t := new(T)
	t.p = &t.a
	fmt.Print(-t.a)
	fmt.Print(!t.b)
	fmt.Print(*t.p)
}
