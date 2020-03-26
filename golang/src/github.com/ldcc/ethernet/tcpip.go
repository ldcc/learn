package ethernet

import (
	"encoding/binary"
	"github.com/ldcc/ethernet/proto"
)

type TCP_Packet struct {
	eth eth_II
	ip  ip_hd
	tcp tcp_hd
}

type eth_II struct {
	mac_dst [6]byte
	mac_src [6]byte
	ip_type [2]byte
}

type ip_hd struct {
	ver_hl   [1]byte // version(1-4), hdr_len(5-8)
	dsfield  [1]byte // dsfield.dscp(1-6), dsfield.ecn(7-8)
	len      [2]byte
	id       [2]byte
	flags    [2]byte // flags.(rb, df, mf)(each 1), frag_offset(4-16)
	ttl      [1]byte
	proto    [1]byte
	checksum [2]byte
	src      [4]byte
	dst      [4]byte
	options  options // (hdr_len - 5) * 32 bit
	data     []byte
}

type tcp_hd struct {
	srcport  [2]byte
	dstport  [2]byte
	seq      [4]byte
	ack      [4]byte
	offset   [1]byte // hdr_len(1-4), flags.res(5-7), flags.ns(8th)
	flags    [1]byte // flags.(cwr, enc, urg, ack, psh, rst, syn, fin)(each 1)
	window   [2]byte
	checksum [2]byte
	urg_ptr  [2]byte
	options  options // (hdr_len - 5) * 32 bit
	data     []byte
}

// Three-Way handshake to establishment

func (*TCP_Packet) TCPHandshake1() {

}

func (*TCP_Packet) TCPHandshake2() {

}

func (*TCP_Packet) TCPHandshake3() {

}

// Four times teardown to termination

func (*TCP_Packet) TCPTeardown1() {

}

func (*TCP_Packet) TCPTeardown2() {

}

func (*TCP_Packet) TCPTeardown3() {

}

func (*TCP_Packet) TCPTeardown4() {

}

////////////////////////////////////////////////
// TCP Options field
////////////////////////////////////////////////

type options = []*option

type option struct {
	kind [1]byte
}

func (*option) packed() []byte {
	// TODO make serialize for each option field
	return []byte{}
}

func unpacked(opt interface{}) *option {
	// TODO unserialize
	return &option{}
}

// TCP Option - No-Operation
type nop struct {
	kind [1]byte
}

func NewNop() *nop {
	return &nop{
		kind: [1]byte{proto.TCP_OPTION_NOP},
	}
}

// TCP Option - Maximum segment size
type mss struct {
	kind [1]byte
	len  [1]byte
	val  [2]byte
}

func NewMss(val uint16) *mss {
	var _val [2]byte
	binary.BigEndian.PutUint16(_val[:], val)
	return &mss{
		kind: [1]byte{proto.TCP_OPTION_MSS},
		len:  [1]byte{proto.TCP_OPTION_MSS_LEN},
		val:  _val,
	}
}

// TCP Option - Window scale (multiply by 128)
type wscale struct {
	kind  [1]byte
	len   [1]byte
	shift [1]byte
}

func NewWscale(shift uint8) *wscale {
	return &wscale{
		kind:  [1]byte{proto.TCP_OPTION_WSCALE},
		len:   [1]byte{proto.TCP_OPTION_WSCALE_LEN},
		shift: [1]byte{shift},
	}
}

// TCP Option - SACK permitted
type sack_perm struct {
	kind [1]byte
	len  [1]byte
}

func NewSackPerm() *sack_perm {
	return &sack_perm{
		kind: [1]byte{proto.TCP_OPTION_SACK_PERM},
		len:  [1]byte{proto.TCP_OPTION_SACK_PERM_LEN},
	}
}

// TCP Option - SACK
type sack struct {
	kind  [1]byte
	len   [1]byte
	edges []byte // each edge has 64 bit, le(1-32) re(33-64)
}

func NewSack(ledge, redge []uint32) *sack {
	var edges []byte
	var le, re [4]byte
	for i := 0; i < len(ledge); i++ {
		binary.BigEndian.PutUint32(le[:], ledge[i])
		binary.BigEndian.PutUint32(re[:], redge[i])
		edges = append(edges, le[:]...)
		edges = append(edges, re[:]...)
	}
	_len := len(edges) + proto.TCP_OPTION_PREFIX_LEN
	return &sack{
		kind:  [1]byte{proto.TCP_OPTION_SACK},
		len:   [1]byte{byte(_len)},
		edges: edges,
	}
}

// TCP Option - Echo (obsoleted by option 8)
type echo struct {
	kind [1]byte
	len  [1]byte
	info [4]byte
}

func NewEcho(info interface{}) *echo {
	var _info [4]byte
	// TODO serialize info to byte array
	return &echo{
		kind: [1]byte{proto.TCP_OPTION_ECHO},
		len:  [1]byte{proto.TCP_OPTION_ECHO_LEN},
		info: _info,
	}
}

// TCP Option - Echo Reply (obsoleted by option 8)
type echo_rep struct {
	kind [1]byte
	len  [1]byte
	info [4]byte
}

func NewEchoRep(info interface{}) *echo_rep {
	var _info [4]byte
	// TODO serialize info to byte array
	return &echo_rep{
		kind: [1]byte{proto.TCP_OPTION_ECHO_REP},
		len:  [1]byte{proto.TCP_OPTION_ECHO_REP_LEN},
		info: _info,
	}
}

// TCP Option - Timestamps: TSval 3440107107, TSecr 3579127637
type timestamp struct {
	kind  [1]byte
	len   [1]byte
	tsval [4]byte
	tsecr [4]byte
}

func NewTimestamp(tsval, tsecr uint32) *timestamp {
	var _tsval, _tsecr [4]byte
	binary.BigEndian.PutUint32(_tsval[:], tsval)
	binary.BigEndian.PutUint32(_tsecr[:], tsecr)
	return &timestamp{
		kind:  [1]byte{proto.TCP_OPTION_TIMESTAMP},
		len:   [1]byte{proto.TCP_OPTION_TIMESTAMP_LEN},
		tsval: _tsval,
		tsecr: _tsecr,
	}
}

// Partial Order Connection Permitted (obsolete)
type poc_perm struct {
	kind [1]byte
	len  [1]byte
}

func NewPocPerm() *poc_perm {
	return &poc_perm{
		kind: [1]byte{proto.TCP_OPTION_POC_PERM},
		len:  [1]byte{proto.TCP_OPTION_POC_PERM_LEN},
	}
}

// Partial Order Service Profile (obsolete)
type poc_sp struct {
	kind   [1]byte
	len    [1]byte
	filler [1]byte // start_flag(1th), end_flag(2th), filler(3-8)
}

func NewPocSp(start_flag, end_flag bool, filler uint8) *poc_sp {
	return &poc_sp{
		kind:   [1]byte{proto.TCP_OPTION_POC_SP},
		len:    [1]byte{proto.TCP_OPTION_POC_SP_LEN},
		filler: [1]byte{},
	}
}
