package main

type human struct {
    name string
}

func main(){
    h := (human{name: "Rickety Cricket"})
    fmt.Println(h.name)
}
