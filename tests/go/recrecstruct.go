package main
import "fmt"
type A struct {b *B}
type B struct {a A}
func main() {
  var a A;
  var b B;
  a.b = &b;
  b.a = a;
  fmt.Print(a,b);
}
