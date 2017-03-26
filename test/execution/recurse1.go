package main;

func main() {
    calculate(5)
}
func calculate(x int) {
    println(x)
    if x == 0 {
      return 0
    } else {
      return calculate(x - 1)
    }
}
