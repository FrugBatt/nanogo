package main

import "fmt"

type L struct {
	x    int
	next *L
}

func main() {
	z := new(L)
	z.x = 42
	fmt.Print(z.x)
	fmt.Print(z.next)
	fmt.Print("\n")
	z.next = new(L)
	n := z.next
	n.x = 43;
	fmt.Print(n.x)
	fmt.Print(z.next.x)
	fmt.Print(z.next.next)
	fmt.Print("\n")
}

