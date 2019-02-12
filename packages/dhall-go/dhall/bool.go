package dhall

import (
	"fmt"
)

// BoolType represents the type of Dhall Bools.
type BoolType struct{}

func (*BoolType) alphaNormalize() Expression { return &BoolType{} }

func (*BoolType) betaNormalize() Expression { return &BoolType{} }

func (*BoolType) equivalent(e Expression) bool {
	r, ok := e.betaNormalize().alphaNormalize().(*BoolType)
	return ok && BoolType{} == *r
}

func (*BoolType) infer(Context) (Expression, error) { return &Type{}, nil }

func (*BoolType) render() string { return "Bool" }

func (*BoolType) renderBinary() binary { return binary{value: "Bool"} }

func (*BoolType) renderCBOR() string { return fmt.Sprintf("%q", "Bool") }

func (bt *BoolType) renderElm() (string, error) { return "Bool", nil }

func (bt *BoolType) renderGo() (string, error) { return "bool", nil }

func (bt *BoolType) renderHaskell() (string, error) { return "Bool", nil }

func (bt *BoolType) renderJSON() (string, error) {
	return "", &JSONError{
		expression: bt,
		message:    "Cannot render type `Bool` to JSON",
	}
}

func (bt *BoolType) renderJSONSchema() (string, error) {
	return fmt.Sprintf("{%q: %q}", "type", "boolean"), nil
}

func (bt *BoolType) renderJavaScript() (string, error) {
	return "", &JavaScriptError{
		expression: bt,
		message:    "Cannot render type `Bool` to JavaScript",
	}
}

func (bt *BoolType) renderPureScript() (string, error) { return "Boolean", nil }

func (bt *BoolType) renderYAML() (string, error) {
	return "", &YAMLError{
		expression: bt,
		message:    "Cannot render type `Bool` to YAML",
	}
}

func (*BoolType) shift(int, string, int) Expression { return &BoolType{} }

func (*BoolType) substitute(string, int, Expression) Expression {
	return &BoolType{}
}

// Bool represents a Dhall Bool.
type Bool struct {
	Value bool
}

func (b *Bool) alphaNormalize() Expression { return b }

func (b *Bool) betaNormalize() Expression { return b }

func (b *Bool) equivalent(e Expression) bool {
	r, ok := e.betaNormalize().alphaNormalize().(*Bool)
	return ok && *b == *r
}

func (*Bool) infer(Context) (Expression, error) { return &BoolType{}, nil }

func (b *Bool) render() string {
	if b.Value {
		return "True"
	}
	return "False"
}

func (b *Bool) renderBinary() binary { return binary{value: b.Value} }

func (b *Bool) renderCBOR() string { return fmt.Sprintf("%t", b.Value) }

func (b *Bool) renderElm() (string, error) {
	if b.Value {
		return "True", nil
	}
	return "False", nil
}

func (b *Bool) renderGo() (string, error) {
	if b.Value {
		return "true", nil
	}
	return "false", nil
}

func (b *Bool) renderHaskell() (string, error) {
	if b.Value {
		return "True", nil
	}
	return "False", nil
}

func (b *Bool) renderJSON() (string, error) {
	if b.Value {
		return "true", nil
	}
	return "false", nil
}

func (b *Bool) renderJSONSchema() (string, error) {
	return fmt.Sprintf("%t", b.Value), nil
}

func (b *Bool) renderJavaScript() (string, error) {
	if b.Value {
		return "true", nil
	}
	return "false", nil
}

func (b *Bool) renderPureScript() (string, error) {
	if b.Value {
		return "true", nil
	}
	return "false", nil
}

func (b *Bool) renderYAML() (string, error) {
	if b.Value {
		return "true", nil
	}
	return "false", nil
}

func (b *Bool) shift(int, string, int) Expression { return b }

func (b *Bool) substitute(string, int, Expression) Expression { return b }

// BoolEqual represents equality of Dhall Bools.
type BoolEqual struct {
	Left  Expression
	Right Expression
}

func (be *BoolEqual) alphaNormalize() Expression {
	return &BoolEqual{
		Left:  be.Left.alphaNormalize(),
		Right: be.Right.alphaNormalize(),
	}
}

func (be *BoolEqual) betaNormalize() Expression {
	l1 := be.Left.betaNormalize()
	switch expression := l1.(type) {
	case *Bool:
		if (*expression == Bool{Value: true}) {
			return be.Right.betaNormalize()
		}
	}
	r1 := be.Right.betaNormalize()
	switch expression := r1.(type) {
	case *Bool:
		if (*expression == Bool{Value: true}) {
			return l1
		}
	}
	if l1.equivalent(r1) {
		return &Bool{Value: true}
	}
	return &BoolEqual{Left: l1, Right: r1}
}

func (be *BoolEqual) equivalent(e Expression) bool {
	l1 := be.betaNormalize().alphaNormalize()
	r1 := e.betaNormalize().alphaNormalize()
	l, lOk := l1.(*BoolEqual)
	r, rOk := r1.(*BoolEqual)
	return (lOk && rOk && *l == *r) || l1.equivalent(r1)
}

func (be *BoolEqual) infer(context Context) (Expression, error) {
	l, err := reduce(be.Left, context)
	if err != nil {
		return nil, err
	}
	r, err := reduce(be.Right, context)
	if err != nil {
		return nil, err
	}
	_, lOk := l.(*BoolType)
	_, rOk := r.(*BoolType)
	if lOk && rOk {
		return &BoolType{}, nil
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

func (be *BoolEqual) render() string {
	return fmt.Sprintf("%s == %s", be.Left.render(), be.Right.render())
}

func (be *BoolEqual) renderBinary() binary {
	l1 := be.Left.renderBinary()
	r1 := be.Right.renderBinary()
	return binary{
		value: [](interface{}){3, 2, l1.value, r1.value},
	}
}

func (be *BoolEqual) renderCBOR() string {
	l1 := be.Left.renderCBOR()
	r1 := be.Right.renderCBOR()
	return fmt.Sprintf("[3, 2, %s, %s]", l1, r1)
}

func (be *BoolEqual) renderElm() (string, error) {
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

func (be *BoolEqual) renderGo() (string, error) {
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

func (be *BoolEqual) renderHaskell() (string, error) {
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

func (be *BoolEqual) renderJSON() (string, error) {
	return "", &JSONError{
		expression: be,
		message:    "Cannot render equality to JSON. Try normalizing first.",
	}
}

func (be *BoolEqual) renderJSONSchema() (string, error) {
	return "", &JSONSchemaError{
		expression: be,
		message:    "Cannot render equality to JSONSchema. Try inferring the type.",
	}
}

func (be *BoolEqual) renderJavaScript() (string, error) {
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

func (be *BoolEqual) renderPureScript() (string, error) {
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

func (be *BoolEqual) renderYAML() (string, error) {
	return "", &YAMLError{
		expression: be,
		message:    "Cannot render equality to YAML. Try normalizing first.",
	}
}

func (be *BoolEqual) shift(d int, x string, m int) Expression {
	l1 := be.Left.shift(d, x, m)
	r1 := be.Right.shift(d, x, m)

	return &BoolEqual{Left: l1, Right: r1}
}

func (be *BoolEqual) substitute(x string, n int, e Expression) Expression {
	l1 := be.Left.substitute(x, n, e)
	r1 := be.Right.substitute(x, n, e)

	return &BoolEqual{Left: l1, Right: r1}
}
