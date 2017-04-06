package main


type human struct {
    name string
    age int
}

func main(){
    h := (human{age: 25, name: "El Duderino"})
    x := h
    fmt.Println(x.name)
    fmt.Println(x.age)
    h.name = "test"
    fmt.Println(h.name)
    fmt.Println(x.name)
}
