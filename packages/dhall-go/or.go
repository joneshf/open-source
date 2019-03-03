package dhall

import (
	"fmt"

	"github.com/joneshf/open-source/packages/go-pretty"
)

// Or represents equality of Dhall Bools.
type Or struct {
	Left  Expression
	Right Expression
}

func (be *Or) alphaNormalize() Expression {
	return &Or{Left: be.Left.alphaNormalize(), Right: be.Right.alphaNormalize()}
}

func (be *Or) betaNormalize() Expression {
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
	if Equivalent(l1, r1) {
		return l1
	}
	return &Or{Left: l1, Right: r1}
}

func (be *Or) infer(context Context) (Expression, error) {
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

func (be *Or) render() pretty.Document {
	return pretty.Spread(be.Left.render(), pretty.Text("||"), be.Right.render())
}

func (be *Or) renderBinary() binary {
	l1 := be.Left.renderBinary()
	r1 := be.Right.renderBinary()
	return binary{value: [](interface{}){3, 0, l1.value, r1.value}}
}

func (be *Or) renderCBOR() pretty.Document {
	return pretty.Append(
		pretty.Text("["),
		pretty.Indent(
			4,
			pretty.Append(
				pretty.Text("3"),
				pretty.Text(","),
				pretty.Line,
				pretty.Text("0"),
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

func (be *Or) renderElm() (string, error) {
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

func (be *Or) renderGo() (string, error) {
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

func (be *Or) renderHaskell() (string, error) {
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

func (be *Or) renderJSON() (string, error) {
	return "", &JSONError{
		expression: be,
		message:    "Cannot render `||` to JSON. Try normalizing first.",
	}
}

func (be *Or) renderJSONSchema() (string, error) {
	return "", &JSONSchemaError{
		expression: be,
		message:    "Cannot render `||` to JSONSchema. Try inferring the type.",
	}
}

func (be *Or) renderJavaScript() (string, error) {
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

func (be *Or) renderPureScript() (string, error) {
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

func (be *Or) renderYAML() (string, error) {
	return "", &YAMLError{
		expression: be,
		message:    "Cannot render `||` to YAML. Try normalizing first.",
	}
}

func (be *Or) shift(d int, x string, m int) Expression {
	return &Or{Left: be.Left.shift(d, x, m), Right: be.Right.shift(d, x, m)}
}

func (be *Or) substitute(x string, n int, e Expression) Expression {
	l1 := be.Left.substitute(x, n, e)
	r1 := be.Right.substitute(x, n, e)

	return &Or{Left: l1, Right: r1}
}
