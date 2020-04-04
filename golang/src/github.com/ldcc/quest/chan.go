package quest

import (
	"fmt"
	"log"
)

type f func() string

func ChanTest1() {
	var ch = make(chan f, 1)
	var rc = make(chan f)
	go func() {
		v := <-ch
		close(ch)
		log.Println("Print1", <-ch)
		log.Println("Print2", <-ch)
		rc <- v
	}()
	go func() {
		select {
		case v := <-rc:
			v()
		}
	}()
	ch <- func() string { return "hello" }
	log.Println("continue...")
	select {}
}

func ChanTest2() {
	// make(chan t) 实例化一个同步阻塞式的 Channel
	var ch = make(chan int)
	go func() {
		for {
			select {
			case i := <-ch:
				fmt.Println(i)
				i++
				ch <- i
			}
		}
	}()
	ch <- 1
	select {}
}

func ChanTest3() {
	// make(chan t) 实例化一个异步非阻塞式的 Channel
	var ch = make(chan int, 1)
	go func() {
		for {
			select {
			case i := <-ch:
				fmt.Println(i)
				i++
				ch <- i
			}
		}
	}()
	ch <- 1
	select {}
}
