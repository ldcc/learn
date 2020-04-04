package quest

import "fmt"

type Int int

func CatchTest1() {
	var a Int = 1
	var i interface{} = a
	var j interface{} = &a

	t, y := i.(Int)
	u, i := i.(*Int)
	fmt.Println(t, y, u, i)

	q, w := j.(Int)
	e, r := j.(*Int)
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

func rec() {
	fmt.Println("recover")
	recover()
}

// 该例子说明了 panic 被设计成所有 defer 之后才执行的，并且演示了里层的 panic 能够被外层 recover 捕获。
// 该例子同样说明了每个 defer 栈都是独立运行于各自 env 且互不干涉的，
// 因为在里函数捕获该 panic 时，函数外可以在上次执行处继续往下执行。
func CatchTest2() {
	defer fmt.Println("outside 1")
	defer fmt.Println("outside 2")
	//defer rec()
	func() {
		defer rec()
		defer fmt.Println("inside")
		panic("panic")
	}()

	fmt.Println("continue...")
}

// 该例子可以大胆猜测每次 recover 都只能针对当前 env 中发生的 panic 进行捕获，
// 而 recover 之所以可以捕获非当前 env 引发的 panic，只是因为 panic 会跟随调用者在 env 间传播而已。
// 如果猜测正确则可以断言 panic 是非 global 的，而是像 defer 栈一样独立运行于 env 上的。
func CatchTest3() {
	func() {
		{
			// 此处使用 rec 和直接使用 recover 会出现不同的结果，
			// 可以推断 defer 是采用「应用序」而非「正则序」进行求值的。
			//defer rec()     // 捕获 panic
			//defer recover() // 无法捕获 panic
		}
		defer func() {
			// 此处使用 rec 和直接使用 recover 也会出现不同的结果，
			// 可以推断 panic 实际上无法污染给新创建的 env。
			// 基于该结论，可以证实 panic 确实是非 global 的，
			// 同时可以反推出 panic 只能在 panic 前的所有历史调用处传播。
			rec()     // 无法捕获 panic
			//recover() // 捕获 panic
		}()
		panic("panic")
	}()
	fmt.Println("done")
}
