package main
import "fmt";

type circle struct {
    radius int
}

func (c circle) diameter() int {
    return c.radius * 2
}

func main() {
}
