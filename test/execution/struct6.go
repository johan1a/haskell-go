package main
import "fmt"
import "fmt"

type human struct {
    name string
    age int
}

func main(){
    h := (human{age: 25, name: "El Duderino"})
    fmt.Println(h.name)
    fmt.Println(h.age)
}
