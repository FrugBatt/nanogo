package main
import "fmt"

type T struct { Print int }
func dis(x int) { fmt.Print(x, "\n") }
func main() { var fmt T; fmt.Print = 42; dis(fmt.Print) }
