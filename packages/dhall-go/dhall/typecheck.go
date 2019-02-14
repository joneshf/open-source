package dhall

import (
	"fmt"
)

// TypeError represents failures when inferring the type of an expression.
type TypeError struct {
	context Context
	message string
}

func (e *TypeError) Error() string {
	return fmt.Sprintf("%s. Context: %s.", e.message, e.context)
}

// Reduce is identical to Expression.infer
// except that it returns the inferred type in normal form.
func Reduce(a Expression) (Expression, error) { return reduce(a, emptyContext) }

func reduce(a Expression, Γ Context) (Expression, error) {
	A0, err := a.infer(Γ)
	if err != nil {
		return nil, err
	}
	return A0.betaNormalize(), nil
}
