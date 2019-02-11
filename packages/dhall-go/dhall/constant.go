package dhall

import (
	"fmt"
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

func (*Kind) equivalent(e Expression) bool {
	r, ok := e.betaNormalize().alphaNormalize().(*Kind)
	return ok && Kind{} == *r
}

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

func (*Kind) render() string { return "Kind" }

func (*Kind) renderBinary() binary { return binary{value: "Kind"} }

func (*Kind) renderCBOR() string { return fmt.Sprintf("%q", "Kind") }

func (k *Kind) renderJSON() (string, error) {
	return "", &JSONError{expression: k, message: "Cannot render `Kind` to JSON"}
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

func (*Sort) equivalent(e Expression) bool {
	r, ok := e.betaNormalize().alphaNormalize().(*Sort)
	return ok && Sort{} == *r
}

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

func (*Sort) render() string { return "Sort" }

func (*Sort) renderBinary() binary { return binary{value: "Sort"} }

func (*Sort) renderCBOR() string { return fmt.Sprintf("%q", "Sort") }

func (k *Sort) renderJSON() (string, error) {
	return "", &JSONError{expression: k, message: "Cannot render `Sort` to JSON"}
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

func (*Type) equivalent(e Expression) bool {
	r, ok := e.betaNormalize().alphaNormalize().(*Type)
	return ok && Type{} == *r
}

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

func (*Type) render() string { return "Type" }

func (*Type) renderBinary() binary { return binary{value: "Type"} }

func (*Type) renderCBOR() string { return fmt.Sprintf("%q", "Type") }

func (k *Type) renderJSON() (string, error) {
	return "", &JSONError{expression: k, message: "Cannot render `Type` to JSON"}
}

func (k *Type) renderYAML() (string, error) {
	return "", &YAMLError{expression: k, message: "Cannot render `Type` to YAML"}
}

func (*Type) shift(int, string, int) Expression { return &Type{} }

func (*Type) substitute(string, int, Expression) Expression { return &Type{} }
