package main;

func main() {
    var a int = 4
    func1(a)
}
func func1(x int) {
    println(2323)
    return func2(x)
}
func func2(y int) {
    println (y)
    return y
    
}
