package dhall

import (
	"fmt"

	"github.com/joneshf/open-source/packages/go-pretty"
)

// Constant represents Dhall Constants
type Constant interface {
	functionCheck(Constant) (Constant, error)
}

// FunctionCheckError represents failures when determining function types.
type FunctionCheckError struct {
	constant Constant
	message  string
}

func (e *FunctionCheckError) Error() string {
	return fmt.Sprintf("%s. Constant: %s.", e.message, e.constant)
}

// Kind represents the type of Dhall types.
type Kind struct{}

func (*Kind) alphaNormalize() Expression { return &Kind{} }

func (*Kind) betaNormalize() Expression { return &Kind{} }

func (*Kind) functionCheck(c Constant) (Constant, error) {
	switch c.(type) {
	case *Kind:
		return &Kind{}, nil
	case *Sort:
		return nil, &FunctionCheckError{
			constant: c,
			message:  "Dependent types not supported for `Kind`",
		}
	case *Type:
		return &Type{}, nil
	default:
		return nil, &FunctionCheckError{
			constant: c,
			message:  "Unhandled function check case for `Kind`",
		}
	}
}

func (*Kind) infer(Context) (Expression, error) { return &Sort{}, nil }

func (*Kind) render() pretty.Document { return pretty.Text("Kind") }

func (*Kind) renderBinary() binary { return binary{value: "Kind"} }

func (*Kind) renderCBOR() string { return fmt.Sprintf("%q", "Kind") }

func (k *Kind) renderElm() (string, error) {
	return "", &ElmError{expression: k, message: "Cannot render `Kind` to Elm"}
}

func (k *Kind) renderGo() (string, error) {
	return "", &GoError{expression: k, message: "Cannot render `Kind` to Go"}
}

func (k *Kind) renderHaskell() (string, error) {
	return "", &HaskellError{
		expression: k,
		message:    "Cannot render `Kind` to Haskell",
	}
}

func (k *Kind) renderJSON() (string, error) {
	return "", &JSONError{expression: k, message: "Cannot render `Kind` to JSON"}
}

func (k *Kind) renderJSONSchema() (string, error) {
	return "", &JSONSchemaError{
		expression: k,
		message:    "Cannot render `Kind` to JSONSchema",
	}
}

func (k *Kind) renderJavaScript() (string, error) {
	return "", &JavaScriptError{
		expression: k,
		message:    "Cannot render `Kind` to JavaScript",
	}
}

func (k *Kind) renderPureScript() (string, error) {
	return "", &PureScriptError{
		expression: k,
		message:    "Cannot render `Kind` to PureScript",
	}
}

func (k *Kind) renderYAML() (string, error) {
	return "", &YAMLError{expression: k, message: "Cannot render `Kind` to YAML"}
}

func (*Kind) shift(int, string, int) Expression { return &Kind{} }

func (*Kind) substitute(string, int, Expression) Expression { return &Kind{} }

// Sort represents the type of Dhall kinds.
type Sort struct{}

func (*Sort) alphaNormalize() Expression { return &Sort{} }

func (*Sort) betaNormalize() Expression { return &Sort{} }

func (*Sort) functionCheck(c Constant) (Constant, error) {
	switch c.(type) {
	case *Kind:
		return &Kind{}, nil
	case *Sort:
		return &Sort{}, nil
	case *Type:
		return &Type{}, nil
	default:
		return nil, &FunctionCheckError{
			constant: c,
			message:  "Unhandled function check case for `Sort`",
		}
	}
}

func (*Sort) infer(context Context) (Expression, error) {
	return nil, &TypeError{
		context: context,
		message: "Cannot infer type of `Sort`",
	}
}

func (*Sort) render() pretty.Document { return pretty.Text("Sort") }

func (*Sort) renderBinary() binary { return binary{value: "Sort"} }

func (*Sort) renderCBOR() string { return fmt.Sprintf("%q", "Sort") }

func (k *Sort) renderElm() (string, error) {
	return "", &ElmError{expression: k, message: "Cannot render `Sort` to Elm"}
}

func (k *Sort) renderGo() (string, error) {
	return "", &GoError{expression: k, message: "Cannot render `Sort` to Go"}
}

func (k *Sort) renderHaskell() (string, error) {
	return "", &HaskellError{
		expression: k,
		message:    "Cannot render `Sort` to Haskell",
	}
}

func (k *Sort) renderJSON() (string, error) {
	return "", &JSONError{expression: k, message: "Cannot render `Sort` to JSON"}
}

func (k *Sort) renderJSONSchema() (string, error) {
	return "", &JSONSchemaError{
		expression: k,
		message:    "Cannot render `Sort` to JSONSchema",
	}
}

func (k *Sort) renderJavaScript() (string, error) {
	return "", &JavaScriptError{
		expression: k,
		message:    "Cannot render `Sort` to JavaScript",
	}
}

func (k *Sort) renderPureScript() (string, error) {
	return "", &PureScriptError{
		expression: k,
		message:    "Cannot render `Sort` to PureScript",
	}
}

func (k *Sort) renderYAML() (string, error) {
	return "", &YAMLError{expression: k, message: "Cannot render `Sort` to YAML"}
}

func (*Sort) shift(int, string, int) Expression { return &Sort{} }

func (*Sort) substitute(string, int, Expression) Expression { return &Sort{} }

// Type represents the type of Dhall terms.
type Type struct{}

func (*Type) alphaNormalize() Expression { return &Type{} }

func (*Type) betaNormalize() Expression { return &Type{} }

func (*Type) functionCheck(c Constant) (Constant, error) {
	switch c.(type) {
	case *Kind:
		return nil, &FunctionCheckError{
			constant: c,
			message:  "Dependent types not supported for `Type`",
		}
	case *Sort:
		return nil, &FunctionCheckError{
			constant: c,
			message:  "Dependent types not supported for `Type`",
		}
	case *Type:
		return &Type{}, nil
	default:
		return nil, &FunctionCheckError{
			constant: c,
			message:  "Unhandled function check case for `Type`",
		}
	}
}

func (*Type) infer(Context) (Expression, error) { return &Kind{}, nil }

func (*Type) render() pretty.Document { return pretty.Text("Type") }

func (*Type) renderBinary() binary { return binary{value: "Type"} }

func (*Type) renderCBOR() string { return fmt.Sprintf("%q", "Type") }

func (k *Type) renderElm() (string, error) {
	return "", &ElmError{expression: k, message: "Cannot render `Type` to Elm"}
}

func (k *Type) renderGo() (string, error) {
	return "", &GoError{expression: k, message: "Cannot render `Type` to Go"}
}

func (k *Type) renderHaskell() (string, error) { return "Type", nil }

func (k *Type) renderJSON() (string, error) {
	return "", &JSONError{expression: k, message: "Cannot render `Type` to JSON"}
}

func (k *Type) renderJSONSchema() (string, error) {
	return "", &JSONSchemaError{
		expression: k,
		message:    "Cannot render `Type` to JSONSchema",
	}
}

func (k *Type) renderJavaScript() (string, error) {
	return "", &JavaScriptError{
		expression: k,
		message:    "Cannot render `Type` to JavaScript",
	}
}

func (k *Type) renderPureScript() (string, error) { return "Type", nil }

func (k *Type) renderYAML() (string, error) {
	return "", &YAMLError{expression: k, message: "Cannot render `Type` to YAML"}
}

func (*Type) shift(int, string, int) Expression { return &Type{} }

func (*Type) substitute(string, int, Expression) Expression { return &Type{} }
