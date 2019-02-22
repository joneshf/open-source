package dhall

import (
	"fmt"

	"github.com/joneshf/open-source/packages/go-pretty"
)

// Bool represents the type of Dhall Bools.
type Bool struct{}

func (*Bool) alphaNormalize() Expression { return &Bool{} }

func (*Bool) betaNormalize() Expression { return &Bool{} }

func (*Bool) infer(Context) (Expression, error) { return &Type{}, nil }

func (*Bool) render() pretty.Document { return pretty.Text("Bool") }

func (*Bool) renderBinary() binary { return binary{value: "Bool"} }

func (*Bool) renderCBOR() string { return fmt.Sprintf("%q", "Bool") }

func (bt *Bool) renderElm() (string, error) { return "Bool", nil }

func (bt *Bool) renderGo() (string, error) { return "bool", nil }

func (bt *Bool) renderHaskell() (string, error) { return "Bool", nil }

func (bt *Bool) renderJSON() (string, error) {
	return "", &JSONError{
		expression: bt,
		message:    "Cannot render type `Bool` to JSON",
	}
}

func (bt *Bool) renderJSONSchema() (string, error) {
	return fmt.Sprintf("{%q: %q}", "type", "boolean"), nil
}

func (bt *Bool) renderJavaScript() (string, error) {
	return "", &JavaScriptError{
		expression: bt,
		message:    "Cannot render type `Bool` to JavaScript",
	}
}

func (bt *Bool) renderPureScript() (string, error) { return "Boolean", nil }

func (bt *Bool) renderYAML() (string, error) {
	return "", &YAMLError{
		expression: bt,
		message:    "Cannot render type `Bool` to YAML",
	}
}

func (*Bool) shift(int, string, int) Expression { return &Bool{} }

func (*Bool) substitute(string, int, Expression) Expression {
	return &Bool{}
}

// BoolValue represents a Dhall BoolValue.
type BoolValue struct {
	Value bool
}

func (b *BoolValue) alphaNormalize() Expression { return b }

func (b *BoolValue) betaNormalize() Expression { return b }

func (*BoolValue) infer(Context) (Expression, error) { return &Bool{}, nil }

func (b *BoolValue) render() pretty.Document {
	if b.Value {
		return pretty.Text("True")
	}
	return pretty.Text("False")
}

func (b *BoolValue) renderBinary() binary { return binary{value: b.Value} }

func (b *BoolValue) renderCBOR() string { return fmt.Sprintf("%t", b.Value) }

func (b *BoolValue) renderElm() (string, error) {
	if b.Value {
		return "True", nil
	}
	return "False", nil
}

func (b *BoolValue) renderGo() (string, error) {
	if b.Value {
		return "true", nil
	}
	return "false", nil
}

func (b *BoolValue) renderHaskell() (string, error) {
	if b.Value {
		return "True", nil
	}
	return "False", nil
}

func (b *BoolValue) renderJSON() (string, error) {
	if b.Value {
		return "true", nil
	}
	return "false", nil
}

func (b *BoolValue) renderJSONSchema() (string, error) {
	return fmt.Sprintf("%t", b.Value), nil
}

func (b *BoolValue) renderJavaScript() (string, error) {
	if b.Value {
		return "true", nil
	}
	return "false", nil
}

func (b *BoolValue) renderPureScript() (string, error) {
	if b.Value {
		return "true", nil
	}
	return "false", nil
}

func (b *BoolValue) renderYAML() (string, error) {
	if b.Value {
		return "true", nil
	}
	return "false", nil
}

func (b *BoolValue) shift(int, string, int) Expression { return b }

func (b *BoolValue) substitute(string, int, Expression) Expression { return b }
