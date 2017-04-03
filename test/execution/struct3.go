package main

type human struct {
    name string
}

func main(){
    h := (human{"Rickety Cricket"})
    fmt.Println(h.name)
}
