package dhall

import (
	"fmt"

	"github.com/joneshf/open-source/packages/go-pretty"
)

// Equal represents equality of Dhall Bools.
type Equal struct {
	Left  Expression
	Right Expression
}

func (be *Equal) alphaNormalize() Expression {
	return &Equal{
		Left:  be.Left.alphaNormalize(),
		Right: be.Right.alphaNormalize(),
	}
}

func (be *Equal) betaNormalize() Expression {
	l1 := be.Left.betaNormalize()
	switch expression := l1.(type) {
	case *BoolValue:
		if (*expression == BoolValue{Value: true}) {
			return be.Right.betaNormalize()
		}
	}
	r1 := be.Right.betaNormalize()
	switch expression := r1.(type) {
	case *BoolValue:
		if (*expression == BoolValue{Value: true}) {
			return l1
		}
	}
	if Equivalent(l1, r1) {
		return &BoolValue{Value: true}
	}
	return &Equal{Left: l1, Right: r1}
}

func (be *Equal) infer(context Context) (Expression, error) {
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
			"Both arguments to `==` (`%#v` and `%#v`) must have type `Bool`",
			be.Left,
			be.Right,
		),
	}
}

func (be *Equal) render() pretty.Document {
	return pretty.Spread(be.Left.render(), pretty.Text("=="), be.Right.render())
}

func (be *Equal) renderBinary() binary {
	l1 := be.Left.renderBinary()
	r1 := be.Right.renderBinary()
	return binary{
		value: [](interface{}){3, 2, l1.value, r1.value},
	}
}

func (be *Equal) renderCBOR() pretty.Document {
	return pretty.Append(
		pretty.Text("["),
		pretty.Indent(
			4,
			pretty.Append(
				pretty.Text("3"),
				pretty.Text(","),
				pretty.Line,
				pretty.Text("2"),
				pretty.Text(","),
				pretty.Line,
				be.Left.renderCBOR(),
				pretty.Text(","),
				pretty.Line,
				be.Right.renderCBOR(),
			),
		),
		pretty.Line,
		pretty.Text("]"),
	)
}

func (be *Equal) renderElm() (string, error) {
	left, errLeft := be.Left.renderElm()
	if errLeft != nil {
		return "", errLeft
	}
	right, errRight := be.Right.renderElm()
	if errRight != nil {
		return "", errRight
	}
	return fmt.Sprintf("%s == %s", left, right), nil
}

func (be *Equal) renderGo() (string, error) {
	left, errLeft := be.Left.renderGo()
	if errLeft != nil {
		return "", errLeft
	}
	right, errRight := be.Right.renderGo()
	if errRight != nil {
		return "", errRight
	}
	return fmt.Sprintf("%s == %s", left, right), nil
}

func (be *Equal) renderHaskell() (string, error) {
	left, errLeft := be.Left.renderHaskell()
	if errLeft != nil {
		return "", errLeft
	}
	right, errRight := be.Right.renderHaskell()
	if errRight != nil {
		return "", errRight
	}
	return fmt.Sprintf("%s == %s", left, right), nil
}

func (be *Equal) renderJSON() (string, error) {
	return "", &JSONError{
		expression: be,
		message:    "Cannot render `==` to JSON. Try normalizing first.",
	}
}

func (be *Equal) renderJSONSchema() (string, error) {
	return "", &JSONSchemaError{
		expression: be,
		message:    "Cannot render `==` to JSONSchema. Try inferring the type.",
	}
}

func (be *Equal) renderJavaScript() (string, error) {
	left, errLeft := be.Left.renderJavaScript()
	if errLeft != nil {
		return "", errLeft
	}
	right, errRight := be.Right.renderJavaScript()
	if errRight != nil {
		return "", errRight
	}
	return fmt.Sprintf("%s === %s", left, right), nil
}

func (be *Equal) renderPureScript() (string, error) {
	left, errLeft := be.Left.renderPureScript()
	if errLeft != nil {
		return "", errLeft
	}
	right, errRight := be.Right.renderPureScript()
	if errRight != nil {
		return "", errRight
	}
	return fmt.Sprintf("%s == %s", left, right), nil
}

func (be *Equal) renderYAML() (string, error) {
	return "", &YAMLError{
		expression: be,
		message:    "Cannot render `==` to YAML. Try normalizing first.",
	}
}

func (be *Equal) shift(d int, x string, m int) Expression {
	l1 := be.Left.shift(d, x, m)
	r1 := be.Right.shift(d, x, m)

	return &Equal{Left: l1, Right: r1}
}

func (be *Equal) substitute(x string, n int, e Expression) Expression {
	l1 := be.Left.substitute(x, n, e)
	r1 := be.Right.substitute(x, n, e)

	return &Equal{Left: l1, Right: r1}
}
