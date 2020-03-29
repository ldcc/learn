package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"net"
)

func ListenAndServe(address string) {
	// 绑定监听地址
	listener, err := net.Listen("tcp", address)
	if err != nil {
		log.Fatal(fmt.Sprintf("listen err: %v", err))
	}
	defer listener.Close()
	log.Println(fmt.Sprintf("bind: %s, start listening...", address))

	for {
		// Accept 会一直阻塞直到有新的连接建立或者listen中断才会返回
		conn, err := listener.Accept()
		if err != nil {
			// 通常是由于listener被关闭无法继续监听导致的错误
			log.Fatal(fmt.Sprintf("accept err: %v", err))
		}
		// 开启新的 goroutine 处理该连接
		go Handle(conn)
	}
}

func Handle(conn net.Conn) {
	// 使用 bufio 标准库提供的缓冲区功能
	reader := bufio.NewReader(conn)
	for {
		// ReadString 会一直阻塞直到遇到分隔符 '\n'
		msg, err := reader.ReadString('\n')
		if err != nil {
			// 通常遇到的错误是连接中断或被关闭，用io.EOF表示
			if err == io.EOF {
				log.Println("connection close")
			} else {
				log.Println(err)
			}
			return
		}
		b := []byte(msg)
		// 将收到的信息发送给客户端
		_, _ = conn.Write(b)
	}
}

func main() {
	ListenAndServe(":8000")
}
