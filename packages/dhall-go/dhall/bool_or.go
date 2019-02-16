package dhall

import (
	"fmt"
)

// BoolOr represents equality of Dhall Bools.
type BoolOr struct {
	Left  Expression
	Right Expression
}

func (be *BoolOr) alphaNormalize() Expression {
	return &BoolOr{
		Left:  be.Left.alphaNormalize(),
		Right: be.Right.alphaNormalize(),
	}
}

func (be *BoolOr) betaNormalize() Expression {
	l1 := be.Left.betaNormalize()
	l1Expression, ok := l1.(*BoolValue)
	if ok && (*l1Expression == BoolValue{Value: false}) {
		return be.Right.betaNormalize()
	}
	if ok && (*l1Expression == BoolValue{Value: true}) {
		return &BoolValue{Value: true}
	}
	r1 := be.Right.betaNormalize()
	r1Expression, ok := r1.(*BoolValue)
	if ok && (*r1Expression == BoolValue{Value: false}) {
		return l1
	}
	if ok && (*r1Expression == BoolValue{Value: true}) {
		return &BoolValue{Value: true}
	}
	if l1.equivalent(r1) {
		return l1
	}
	return &BoolOr{Left: l1, Right: r1}
}

func (be *BoolOr) equivalent(e Expression) bool {
	l1 := be.betaNormalize().alphaNormalize()
	r1 := e.betaNormalize().alphaNormalize()
	l, lOk := l1.(*BoolOr)
	r, rOk := r1.(*BoolOr)
	return (lOk && rOk && *l == *r) || l1.equivalent(r1)
}

func (be *BoolOr) infer(context Context) (Expression, error) {
	l, err := reduce(be.Left, context)
	if err != nil {
		return nil, err
	}
	r, err := reduce(be.Right, context)
	if err != nil {
		return nil, err
	}
	_, lOk := l.(*Bool)
	_, rOk := r.(*Bool)
	if lOk && rOk {
		return &Bool{}, nil
	}
	return nil, &TypeError{
		context: context,
		message: fmt.Sprintf(
			"Both arguments to `||` (`%#v` and `%#v`) must have type `Bool`",
			be.Left,
			be.Right,
		),
	}
}

func (be *BoolOr) render() string {
	return fmt.Sprintf("%s || %s", be.Left.render(), be.Right.render())
}

func (be *BoolOr) renderBinary() binary {
	l1 := be.Left.renderBinary()
	r1 := be.Right.renderBinary()
	return binary{
		value: [](interface{}){3, 0, l1.value, r1.value},
	}
}

func (be *BoolOr) renderCBOR() string {
	l1 := be.Left.renderCBOR()
	r1 := be.Right.renderCBOR()
	return fmt.Sprintf("[3, 0, %s, %s]", l1, r1)
}

func (be *BoolOr) renderElm() (string, error) {
	left, errLeft := be.Left.renderElm()
	if errLeft != nil {
		return "", errLeft
	}
	right, errRight := be.Right.renderElm()
	if errRight != nil {
		return "", errRight
	}
	return fmt.Sprintf("%s || %s", left, right), nil
}

func (be *BoolOr) renderGo() (string, error) {
	left, errLeft := be.Left.renderGo()
	if errLeft != nil {
		return "", errLeft
	}
	right, errRight := be.Right.renderGo()
	if errRight != nil {
		return "", errRight
	}
	return fmt.Sprintf("%s || %s", left, right), nil
}

func (be *BoolOr) renderHaskell() (string, error) {
	left, errLeft := be.Left.renderHaskell()
	if errLeft != nil {
		return "", errLeft
	}
	right, errRight := be.Right.renderHaskell()
	if errRight != nil {
		return "", errRight
	}
	return fmt.Sprintf("%s || %s", left, right), nil
}

func (be *BoolOr) renderJSON() (string, error) {
	return "", &JSONError{
		expression: be,
		message:    "Cannot render `||` to JSON. Try normalizing first.",
	}
}

func (be *BoolOr) renderJSONSchema() (string, error) {
	return "", &JSONSchemaError{
		expression: be,
		message:    "Cannot render `||` to JSONSchema. Try inferring the type.",
	}
}

func (be *BoolOr) renderJavaScript() (string, error) {
	left, errLeft := be.Left.renderJavaScript()
	if errLeft != nil {
		return "", errLeft
	}
	right, errRight := be.Right.renderJavaScript()
	if errRight != nil {
		return "", errRight
	}
	return fmt.Sprintf("%s || %s", left, right), nil
}

func (be *BoolOr) renderPureScript() (string, error) {
	left, errLeft := be.Left.renderPureScript()
	if errLeft != nil {
		return "", errLeft
	}
	right, errRight := be.Right.renderPureScript()
	if errRight != nil {
		return "", errRight
	}
	return fmt.Sprintf("%s || %s", left, right), nil
}

func (be *BoolOr) renderYAML() (string, error) {
	return "", &YAMLError{
		expression: be,
		message:    "Cannot render `||` to YAML. Try normalizing first.",
	}
}

func (be *BoolOr) shift(d int, x string, m int) Expression {
	l1 := be.Left.shift(d, x, m)
	r1 := be.Right.shift(d, x, m)

	return &BoolOr{Left: l1, Right: r1}
}

func (be *BoolOr) substitute(x string, n int, e Expression) Expression {
	l1 := be.Left.substitute(x, n, e)
	r1 := be.Right.substitute(x, n, e)

	return &BoolOr{Left: l1, Right: r1}
}
