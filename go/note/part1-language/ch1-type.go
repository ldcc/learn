package part1

import (
	"fmt"
	"unsafe"
)

// 1.1 variable

// define by `var`
var _ int

// type inference
var _ float32 = 1.6
var _ = "abc"

// define multiple
var _, _, _ int
var _, _ = "abc", 123
var (
	_ int
	_         = "abc"
	_ float32 = 23.2
)

func Test1p1() {
	// define by `:=`
	x, y := 123, "Hello, World!"

	// unused variable `z` triggering compiled error
	// z := x + len(y)
	_ = x + len(y)

	// variable closure
	{
		x := 1
		fmt.Println(x)
	}
	fmt.Println(x)
}

// 1.2 constant

// define constant (multiple, type inference)
const _, _ float32 = 1.0, 2.9
const _ = "Hello, World!"
const (
	_            = false
	_, _ float32 = 10.32, 100
)

// ignore type and initial value
const (
	_ = "abc"
	_  // ⇒ "abc"
)

// enumerator `iota`
const (
	Sunday    = iota // 0
	Monday           // 1
	Tuesday          // 2
	Wednesday        // 3
	Thursday         // 4
	Friday           // 5
	Saturday         // 6
)

// `iota` describe the index which in constant group
const (
	A = iota // 0
	B        // 1
	C = "c"  // c
	D        // c
	E = iota // 4
	F        // 5
)

func Test1p2() {
	// unused constant x compiled safety
	const x = "abc"

	// constant `y` overflows triggering compile error
	//const y byte = 256

	// function return value which result determinable during compiling
	const (
		f1 = "abc"
		f2 = len(f1)
		_  = unsafe.Sizeof(f2)
	)
	fmt.Println(A, B, C, D, E, F)
}
