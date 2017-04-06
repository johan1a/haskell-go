package main
import "fmt";

func main() {
    var k int = 23
    var l int = 1
    l = swoosh(k)
    fmt.Println(k)
    fmt.Println(l)
}
func swoosh(x int) {
    return 2 + x
}
