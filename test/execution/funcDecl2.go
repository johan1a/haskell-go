package main;

func main() {
    var a int = 4
    func1(a)
    println(12345)
}
func func1(x int) {
    println(2323)
    return func2(x + 1)
}
func func2(y int) {
    println(y)
    return 1
    
}
