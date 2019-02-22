package dhall

import (
	"fmt"

	"github.com/joneshf/open-source/packages/go-pretty"
)

// NotEqual represents equality of Dhall Bools.
type NotEqual struct {
	Left  Expression
	Right Expression
}

func (be *NotEqual) alphaNormalize() Expression {
	return &NotEqual{
		Left:  be.Left.alphaNormalize(),
		Right: be.Right.alphaNormalize(),
	}
}

func (be *NotEqual) betaNormalize() Expression {
	l1 := be.Left.betaNormalize()
	switch expression := l1.(type) {
	case *BoolValue:
		if (*expression == BoolValue{Value: false}) {
			return be.Right.betaNormalize()
		}
	}
	r1 := be.Right.betaNormalize()
	switch expression := r1.(type) {
	case *BoolValue:
		if (*expression == BoolValue{Value: false}) {
			return l1
		}
	}
	if Equivalent(l1, r1) {
		return &BoolValue{Value: false}
	}
	return &NotEqual{Left: l1, Right: r1}
}

func (be *NotEqual) infer(context Context) (Expression, error) {
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
			"Both arguments to `!=` (`%#v` and `%#v`) must have type `Bool`",
			be.Left,
			be.Right,
		),
	}
}

func (be *NotEqual) render() pretty.Document {
	return pretty.Spread(be.Left.render(), pretty.Text("!="), be.Right.render())
}

func (be *NotEqual) renderBinary() binary {
	l1 := be.Left.renderBinary()
	r1 := be.Right.renderBinary()
	return binary{
		value: [](interface{}){3, 3, l1.value, r1.value},
	}
}

func (be *NotEqual) renderCBOR() string {
	l1 := be.Left.renderCBOR()
	r1 := be.Right.renderCBOR()
	return fmt.Sprintf("[3, 3, %s, %s]", l1, r1)
}

func (be *NotEqual) renderElm() (string, error) {
	left, errLeft := be.Left.renderElm()
	if errLeft != nil {
		return "", errLeft
	}
	right, errRight := be.Right.renderElm()
	if errRight != nil {
		return "", errRight
	}
	return fmt.Sprintf("%s /= %s", left, right), nil
}

func (be *NotEqual) renderGo() (string, error) {
	left, errLeft := be.Left.renderGo()
	if errLeft != nil {
		return "", errLeft
	}
	right, errRight := be.Right.renderGo()
	if errRight != nil {
		return "", errRight
	}
	return fmt.Sprintf("%s != %s", left, right), nil
}

func (be *NotEqual) renderHaskell() (string, error) {
	left, errLeft := be.Left.renderHaskell()
	if errLeft != nil {
		return "", errLeft
	}
	right, errRight := be.Right.renderHaskell()
	if errRight != nil {
		return "", errRight
	}
	return fmt.Sprintf("%s /= %s", left, right), nil
}

func (be *NotEqual) renderJSON() (string, error) {
	return "", &JSONError{
		expression: be,
		message:    "Cannot render `!=` to JSON. Try normalizing first.",
	}
}

func (be *NotEqual) renderJSONSchema() (string, error) {
	return "", &JSONSchemaError{
		expression: be,
		message:    "Cannot render `!=` to JSONSchema. Try inferring the type.",
	}
}

func (be *NotEqual) renderJavaScript() (string, error) {
	left, errLeft := be.Left.renderJavaScript()
	if errLeft != nil {
		return "", errLeft
	}
	right, errRight := be.Right.renderJavaScript()
	if errRight != nil {
		return "", errRight
	}
	return fmt.Sprintf("%s !== %s", left, right), nil
}

func (be *NotEqual) renderPureScript() (string, error) {
	left, errLeft := be.Left.renderPureScript()
	if errLeft != nil {
		return "", errLeft
	}
	right, errRight := be.Right.renderPureScript()
	if errRight != nil {
		return "", errRight
	}
	return fmt.Sprintf("%s /= %s", left, right), nil
}

func (be *NotEqual) renderYAML() (string, error) {
	return "", &YAMLError{
		expression: be,
		message:    "Cannot render `!=` to YAML. Try normalizing first.",
	}
}

func (be *NotEqual) shift(d int, x string, m int) Expression {
	l1 := be.Left.shift(d, x, m)
	r1 := be.Right.shift(d, x, m)

	return &NotEqual{Left: l1, Right: r1}
}

func (be *NotEqual) substitute(x string, n int, e Expression) Expression {
	l1 := be.Left.substitute(x, n, e)
	r1 := be.Right.substitute(x, n, e)

	return &NotEqual{Left: l1, Right: r1}
}
