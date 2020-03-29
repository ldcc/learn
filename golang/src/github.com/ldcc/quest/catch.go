package quest

import "fmt"

type Int int

func catch_test() {
	var a Int = 1
	var i interface{} = a
	var j interface{} = &a

	t, y := i.(Int)
	u, i := i.(*Int)

	q, w := j.(Int)
	e, r := j.(*Int)

	fmt.Println(t, y, u, i)

	fmt.Println(q, w, e, r)

	err := a.ppp()
	fmt.Println(err)

	fmt.Println("continue...")
}

func (a Int) add1(b Int) (sum Int, err interface{}) {
	defer func() {
		err = recover()
	}()

	sum = a + b
	return
}

func (a *Int) add2(b Int) (sum Int, err interface{}) {
	defer func() {
		err = recover()
	}()

	sum = *a + b
	return
}

func (a *Int) ppp() (err interface{}) {
	defer func() {
		err = recover()
	}()

	var i interface{} = a
	_ = i.(Int)
	return
}
