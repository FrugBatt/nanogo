package main

import "fmt"

func main() {
	x := ""
	s := &x
	fmt.Print(4, *s, 2, "\n")
	x = "Dave"
	s = &x
	fmt.Print("I'm sorry, ", *s, ". I'm afraid I can't do that.\n")
}

