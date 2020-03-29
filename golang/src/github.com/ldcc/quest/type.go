package quest

import "C"
import (
	"encoding/json"
	"fmt"
	"log"
)

// #################################################################
// Instance #1
// #################################################################

func TypeTest1() {
	TestGetACat()
}

type Cat interface {
	Meow()
}
type Dotty struct{}

func (*Dotty) Meow() {
	fmt.Println("meow")
}

type Tabby struct{}

func (*Tabby) Meow() {
	fmt.Println("meow")
}

func GetACat() Cat {
	var myTabby *Tabby
	// Oops, we forgot to set myTabby to a real value
	return myTabby
}

func TestGetACat() {
	cat := GetACat()
	defer func() {
		err := recover()
		log.Fatalf("\n%v\n%s", cat, err)
	}()
	if cat == nil {
		log.Fatalln("Forgot to return a real cat!")
	} else {
		var dotty = cat.(*Dotty)
		log.Println(dotty)
	}
}

// #################################################################
// Instance #2
// #################################################################

// 所谓接口查询，即： If \Gamma\vdash v \in \sigma, Is \Gamma\vdash v \in \tau

func TypeTest2() {
	var w W = &io{}
	w.Write("write1")
	w.Write("write2")
	val, ok := w.(R)
	if !ok {
		fmt.Println("eclipse..")
	} else {
		val.Read()
		val.Read()
	}
}

type R interface {
	Read()
}

type W interface {
	Write(name string)
}

type RW interface {
	R
	W
}

type io struct {
	name []string
	r    int
}

func (t *io) Read() {
	if len(t.name) > t.r {
		fmt.Println(t.name[t.r])
		t.r++
	} else {
		fmt.Println("empty")
	}
}
func (t *io) Write(name string) {
	t.name = append(t.name, name)
	fmt.Println("wirte success.", t.name)

}

// #################################################################
// Instance #3
// #################################################################

func TypeTest3() {
	var a *int
	ja, err := json.Marshal(a)
	fmt.Println(ja, err)

	var foo = func() {}
	fa, err := json.Marshal(foo)
	fmt.Println(fa, err)

	var pine chan int
	pa, err := json.Marshal(pine)
	fmt.Println(pa, err)

	var cn complex64
	ca, err := json.Marshal(cn)
	fmt.Println(ca, err)

	var n int
	na, err := json.Marshal(n)
	fmt.Println(na, err)
}