package ethernet

type HTTP_Packet struct {
	tcp  TCP_Packet
	http http_hd
}

type http_hd struct {
	reqmth [3]byte // request.method
	requri []byte  // request.uri
	reqver [8]byte // request.version
	accept []byte
	conn   []byte
	text   []byte
}
