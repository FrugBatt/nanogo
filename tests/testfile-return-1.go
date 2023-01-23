package main
import "fmt"
func foo(x int) int { if x > 0 { return 1 } else { return 2 } }
func main() {
  fmt.Print(foo(1), foo(-1));
}
