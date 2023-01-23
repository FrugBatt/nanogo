package main
import "fmt"

type A struct { x string }

type B struct {
  a A;
  x string;
}

func main() {
  var e1 B;
  var e2 A;
  e2.x = "structure e2";
  e1.x = "structure e1";
  e1.a = e2;

  p1 := &e2;
  p2 := &e1.a;

  p1.x = "pointeur 1";
  p2.x = "pointeur 2";

  fmt.Print(e1,e2);
}
