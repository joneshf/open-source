package dhall

import (
	"github.com/ugorji/go/codec"
)

// Encode attempts to convert an Expression to its binary encoding.
func Encode(handle *codec.CborHandle, expr Expression) ([]byte, error) {
	out := make([]byte, 0)
	encoder := codec.NewEncoderBytes(&out, handle)
	if err := encoder.Encode(expr.encode().value); err != nil {
		return nil, err
	}
	return out, nil
}
