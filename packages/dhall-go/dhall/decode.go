package dhall

import (
	"fmt"

	"github.com/ugorji/go/codec"
)

// DecodeError represents failures when attempting to decode to a Dhall expression.
type DecodeError struct {
	message string
	value   interface{}
}

func (e *DecodeError) Error() string {
	return fmt.Sprintf("%s. Raw value: %+v.", e.message, e.value)
}

func hydrate(raw interface{}) (Expression, error) {
	switch rawType := raw.(type) {
	case bool:
		return &Bool{Value: raw.(bool)}, nil
	case string:
		switch rawType {
		case "Bool":
			return &BoolType{}, nil
		case "Kind":
			return &Kind{}, nil
		case "Sort":
			return &Sort{}, nil
		case "Type":
			return &Type{}, nil
		}
	case []interface{}:
		xs := raw.([]interface{})
		if len(xs) == 4 && xs[0].(uint64) == 3 && xs[1].(uint64) == 2 {
			left, err := hydrate(xs[2])
			if err != nil {
				return nil, err
			}
			right, err := hydrate(xs[3])
			if err != nil {
				return nil, err
			}
			return &BoolEqual{Left: left, Right: right}, nil
		}
	}

	return nil, &DecodeError{message: "Unhandled case", value: raw}
}

// Decode attempts to convert a binary encoding to a Dhall expression.
func Decode(handle *codec.CborHandle, in []byte) (Expression, error) {
	var raw interface{}

	decoder := codec.NewDecoderBytes(in, handle)
	if err := decoder.Decode(&raw); err != nil {
		return nil, err
	}

	return hydrate(raw)
}
