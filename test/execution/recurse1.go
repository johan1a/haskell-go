package main;

func main() {
    var abc1 int = 4
    println(abc1)
    calculate(4)
}
func calculate(x int) {
    println(x)
    if x == 0 {
      println(11)
      return 0
    } else {
      println(12)
      return calculate(x - 1)
    }
}
