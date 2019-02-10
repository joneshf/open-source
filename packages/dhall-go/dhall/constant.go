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

func (*Kind) encode() cbor { return cbor{value: "Kind"} }

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

func (*Kind) shift(int, string, int) Expression { return &Kind{} }

func (*Kind) substitute(string, int, Expression) Expression { return &Kind{} }

// Sort represents the type of Dhall kinds.
type Sort struct{}

func (*Sort) alphaNormalize() Expression { return &Sort{} }

func (*Sort) betaNormalize() Expression { return &Sort{} }

func (*Sort) encode() cbor { return cbor{value: "Sort"} }

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

func (*Sort) shift(int, string, int) Expression { return &Sort{} }

func (*Sort) substitute(string, int, Expression) Expression { return &Sort{} }

// Type represents the type of Dhall terms.
type Type struct{}

func (*Type) alphaNormalize() Expression { return &Type{} }

func (*Type) betaNormalize() Expression { return &Type{} }

func (*Type) encode() cbor { return cbor{value: "Type"} }

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

func (*Type) shift(int, string, int) Expression { return &Type{} }

func (*Type) substitute(string, int, Expression) Expression { return &Type{} }
