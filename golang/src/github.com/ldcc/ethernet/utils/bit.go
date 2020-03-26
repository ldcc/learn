package utils

// Bit operation

type BitOrder interface {
	// e := b & ^(1 << n)
	SetN20(n uint, b []byte)
	// e := b | (1 << n)
	SetN21(n uint, b []byte)
	// e := b >> (n - 1) & 1
	GetNBit(n uint, b []byte)
	// e := b ^ (1 << n)
	InvNBit(n uint, b []byte)

	String() string
}

type bit_endian struct {}

var BitEndian bit_endian

func (bo bit_endian) SetN20(n uint8, b []byte) {

}
func (bo bit_endian) SetN21(n uint8, b []byte) {

}
func (bo bit_endian) GetNBit(n uint8, b []byte) {

}
func (bo bit_endian) InvNBit(n uint8, b []byte) {

}
