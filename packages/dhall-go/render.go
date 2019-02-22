package dhall

import (
	"fmt"

	"github.com/ugorji/go/codec"

	"github.com/joneshf/open-source/packages/go-pretty"
)

// ElmError represents errors attempting to render Elm.
type ElmError struct {
	expression Expression
	message    string
}

func (e *ElmError) Error() string {
	return fmt.Sprintf("%s. Expression: %#v", e.message, e.expression.render())
}

// GoError represents errors attempting to render Go.
type GoError struct {
	expression Expression
	message    string
}

func (e *GoError) Error() string {
	return fmt.Sprintf("%s. Expression: %#v", e.message, e.expression.render())
}

// HaskellError represents errors attempting to render Haskell.
type HaskellError struct {
	expression Expression
	message    string
}

func (e *HaskellError) Error() string {
	return fmt.Sprintf("%s. Expression: %#v", e.message, e.expression.render())
}

// JSONError represents errors attempting to render JSON.
type JSONError struct {
	expression Expression
	message    string
}

func (e *JSONError) Error() string {
	return fmt.Sprintf("%s. Expression: %#v", e.message, e.expression.render())
}

// JSONSchemaError represents errors attempting to render JSON.
type JSONSchemaError struct {
	expression Expression
	message    string
}

func (e *JSONSchemaError) Error() string {
	return fmt.Sprintf("%s. Expression: %#v", e.message, e.expression.render())
}

// JavaScriptError represents errors attempting to render JavaScript.
type JavaScriptError struct {
	expression Expression
	message    string
}

func (e *JavaScriptError) Error() string {
	return fmt.Sprintf("%s. Expression: %#v", e.message, e.expression.render())
}

// PureScriptError represents errors attempting to render PureScript.
type PureScriptError struct {
	expression Expression
	message    string
}

func (e *PureScriptError) Error() string {
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
func Render(expression Expression) pretty.Document { return expression.render() }

// RenderBinary attempts to render the given Expression as binary.
func RenderBinary(
	handle *codec.CborHandle,
	expr Expression,
	currentVersion string,
) ([]byte, error) {
	out := make([]byte, 0)
	encoder := codec.NewEncoderBytes(&out, handle)
	withVersion := [](interface{}){currentVersion, expr.renderBinary().value}
	if err := encoder.Encode(withVersion); err != nil {
		return nil, err
	}
	return out, nil
}

// RenderCBOR renders the given Expression as CBOR.
// This rendering can be useful for humans to understand the binary rendering.
func RenderCBOR(expr Expression) string { return expr.renderCBOR() }

// RenderElm attempts to render the given expression as elm.
func RenderElm(expression Expression) (string, error) {
	return expression.renderElm()
}

// RenderGo attempts to render the given expression as Go.
func RenderGo(expression Expression) (string, error) {
	return expression.renderGo()
}

// RenderHaskell attempts to render the given expression as Haskell.
func RenderHaskell(expression Expression) (string, error) {
	return expression.renderHaskell()
}

// RenderJavaScript attempts to render the given expression as JavaScript.
func RenderJavaScript(expression Expression) (string, error) {
	return expression.renderJavaScript()
}

// RenderJSON attempts to render the given expression as JSON.
func RenderJSON(expression Expression) (string, error) {
	return expression.renderJSON()
}

// RenderJSONSchema attempts to render the given expression as JSON.
func RenderJSONSchema(expression Expression) (string, error) {
	return expression.renderJSONSchema()
}

// RenderPureScript attempts to render the given expression as PureScript.
func RenderPureScript(expression Expression) (string, error) {
	return expression.renderPureScript()
}

// RenderYAML attempts to render the given expression as YAML.
func RenderYAML(expression Expression) (string, error) {
	return expression.renderYAML()
}
