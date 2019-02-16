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
		return &BoolValue{Value: raw.(bool)}, nil
	case string:
		switch rawType {
		case "Bool":
			return &Bool{}, nil
		case "Kind":
			return &Kind{}, nil
		case "Sort":
			return &Sort{}, nil
		case "Type":
			return &Type{}, nil
		}
	case []interface{}:
		xs := raw.([]interface{})
		// Operators
		if len(xs) == 4 {
			// BoolOr
			if xs[0].(uint64) == 3 && xs[1].(uint64) == 0 {
				left, err := hydrate(xs[2])
				if err != nil {
					return nil, err
				}
				right, err := hydrate(xs[3])
				if err != nil {
					return nil, err
				}
				return &BoolOr{Left: left, Right: right}, nil
			}
			// BoolEqual
			if xs[0].(uint64) == 3 && xs[1].(uint64) == 2 {
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
			// BoolNotEqual
			if xs[0].(uint64) == 3 && xs[1].(uint64) == 3 {
				left, err := hydrate(xs[2])
				if err != nil {
					return nil, err
				}
				right, err := hydrate(xs[3])
				if err != nil {
					return nil, err
				}
				return &BoolNotEqual{Left: left, Right: right}, nil
			}
		}
	}

	return nil, &DecodeError{message: "Unhandled case", value: raw}
}

// Decode attempts to convert a binary encoding to a Dhall expression.
func Decode(
	handle *codec.CborHandle,
	in []byte,
	currentVersion string,
) (Expression, error) {
	var raw interface{}

	decoder := codec.NewDecoderBytes(in, handle)
	if err := decoder.Decode(&raw); err != nil {
		return nil, err
	}

	withVersion, ok := raw.([](interface{}))
	if !ok || len(withVersion) != 2 {
		return nil, &DecodeError{
			message: "Expected version followed by expression",
			value:   raw,
		}
	}

	rawVersion, rawExpression := withVersion[0], withVersion[1]
	version, ok := rawVersion.(string)
	if rawVersion != currentVersion {
		return nil, &DecodeError{
			message: fmt.Sprintf(
				"Cannot decode different version. Expected version to be %s, but it was %s.",
				currentVersion,
				version,
			),
			value: raw,
		}
	}

	return hydrate(rawExpression)
}
