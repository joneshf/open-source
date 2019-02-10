package dhall

type cbor struct {
	value interface{}
}

// Expression represents any valid Dhall expression.
type Expression interface {
	alphaNormalize() Expression
	betaNormalize() Expression
	encode() cbor
	infer(Context) (Expression, error)
	shift(int, string, int) Expression
	substitute(string, int, Expression) Expression
}

// Equivalent determines the following relationship between Expressions: l ≡ r.
func Equivalent(l0 Expression, r0 Expression) bool {
	l1 := l0.betaNormalize()
	r1 := r0.betaNormalize()
	le := l1.alphaNormalize()
	re := r1.alphaNormalize()
	return le == re
}
