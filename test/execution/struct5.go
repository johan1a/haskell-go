package main

type human struct {
    age int
}

func main(){
    h := (human{age: 25})
    fmt.Println(h.age)
}
