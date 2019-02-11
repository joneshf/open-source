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

// RenderCBOR attempts to render the given Expression as CBOR.
// The CBOR can then be rendered to the binary encoding.
func RenderCBOR(handle *codec.CborHandle, expr Expression) ([]byte, error) {
	out := make([]byte, 0)
	encoder := codec.NewEncoderBytes(&out, handle)
	if err := encoder.Encode(expr.renderCBOR().value); err != nil {
		return nil, err
	}
	return out, nil
}

// RenderJSON attempts to render the given expression as JSON.
func RenderJSON(expression Expression) (string, error) {
	return expression.renderJSON()
}

// RenderYAML attempts to render the given expression as YAML.
func RenderYAML(expression Expression) (string, error) {
	return expression.renderYAML()
}
