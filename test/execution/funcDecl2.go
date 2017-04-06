package main
import "fmt";

func main() {
    var a int = 4
    fmt.Println(func1(a))
    fmt.Println(12345)
}
func func1(x int) {
    fmt.Println(2323)
    return func2(x + 1)
}
func func2(y int) {
    fmt.Println(y)
    return 1
    
}
