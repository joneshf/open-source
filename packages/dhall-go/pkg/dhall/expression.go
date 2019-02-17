package dhall

type binary struct {
	value interface{}
}

// Expression represents any valid Dhall expression.
type Expression interface {
	alphaNormalize() Expression
	betaNormalize() Expression
	equivalent(Expression) bool
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
func Equivalent(l Expression, r Expression) bool {
	return l.equivalent(r)
}

// Normalize performs beta normalization followed by alpha normalization.
func Normalize(e Expression) Expression {
	return e.betaNormalize().alphaNormalize()
}
