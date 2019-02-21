package dhall

import (
	"reflect"
)

type binary struct {
	value interface{}
}

// Expression represents any valid Dhall expression.
type Expression interface {
	alphaNormalize() Expression
	betaNormalize() Expression
	infer(Context) (Expression, error)
	render() string
	renderBinary() binary
	renderCBOR() string
	renderElm() (string, error)
	renderGo() (string, error)
	renderHaskell() (string, error)
	renderJSON() (string, error)
	renderJSONSchema() (string, error)
	renderJavaScript() (string, error)
	renderPureScript() (string, error)
	renderYAML() (string, error)
	shift(int, string, int) Expression
	substitute(string, int, Expression) Expression
}

// Equivalent determines the following relationship between Expressions: l ≡ r.
func Equivalent(l0 Expression, r0 Expression) bool {
	l1 := l0.betaNormalize().alphaNormalize()
	r1 := r0.betaNormalize().alphaNormalize()
	return reflect.DeepEqual(l1, r1)
}

// Normalize performs beta normalization followed by alpha normalization.
func Normalize(e Expression) Expression {
	return e.betaNormalize().alphaNormalize()
}
