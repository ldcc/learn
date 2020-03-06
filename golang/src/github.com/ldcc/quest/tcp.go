package tcp

import (
	"fmt"
)

type tcp_package struct {
	tcp_header tcp_header
}

type tcp_header struct {
	source_port                  [2]byte
	destination_port             [2]byte
	sequence                     [4]byte
	acknowledgement              [4]byte
	data_offset                  [1]byte
	reserved                     [1]byte
	urg, ack, psh, rst, syn, fin bool
	window                       [2]byte
	checksum                     [2]byte
	urgent_pointer               [2]byte
	options                      []byte // limited in 15
}
