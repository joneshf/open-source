package dhall

import (
	"fmt"

	"github.com/ugorji/go/codec"
)

// JSONError represents errors attempting to render JSON.
type JSONError struct {
	expression Expression
	message    string
}

func (e *JSONError) Error() string {
	return fmt.Sprintf("%s. Expression: %#v", e.message, e.expression.render())
}

// YAMLError represents errors attempting to render YAML.
type YAMLError struct {
	expression Expression
	message    string
}

func (e *YAMLError) Error() string {
	return fmt.Sprintf("%s. Expression: %#v", e.message, e.expression.render())
}

// Render renders the given expression as Dhall source.
func Render(expression Expression) string { return expression.render() }

// RenderBinary attempts to render the given Expression as binary.
func RenderBinary(handle *codec.CborHandle, expr Expression) ([]byte, error) {
	out := make([]byte, 0)
	encoder := codec.NewEncoderBytes(&out, handle)
	if err := encoder.Encode(expr.renderBinary().value); err != nil {
		return nil, err
	}
	return out, nil
}

// RenderCBOR renders the given Expression as CBOR.
// This rendering can be useful for humans to understand the binary rendering.
func RenderCBOR(expr Expression) string { return expr.renderCBOR() }

// RenderJSON attempts to render the given expression as JSON.
func RenderJSON(expression Expression) (string, error) {
	return expression.renderJSON()
}

// RenderYAML attempts to render the given expression as YAML.
func RenderYAML(expression Expression) (string, error) {
	return expression.renderYAML()
}
