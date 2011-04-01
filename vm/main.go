package main

import (
	"os"
	"fmt"
//	"io"
	"./types"
	"./deserializer"
)

func main() {
	f, e := os.Open("./main.cso", os.O_RDONLY, 0666)
	if e != nil {
		return
	}
	d,_ := deserializer.NewReader(f)
	o := d.ReadObject()
	o2 := d.ReadObject()
	types.Write(o)
	fmt.Print("\n")
	types.Write(o2)
	fmt.Print("\n")
}
