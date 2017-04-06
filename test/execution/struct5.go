package main
import "fmt"

type human struct {
    age int
}

func main(){
    h := (human{age: 25})
    fmt.Println(h.age)
}
