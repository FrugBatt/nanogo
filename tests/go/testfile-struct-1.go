package main
import "fmt"
type A struct { b *B }
type B struct { a A }
func main() {
  var a A;
  var b B;
  b.a = a;
  a.b = &b;
  fmt.Print(a, b);
}
