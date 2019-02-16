package dhall

import (
	"fmt"
)

// Bool represents the type of Dhall Bools.
type Bool struct{}

func (*Bool) alphaNormalize() Expression { return &Bool{} }

func (*Bool) betaNormalize() Expression { return &Bool{} }

func (*Bool) equivalent(e Expression) bool {
	r, ok := e.betaNormalize().alphaNormalize().(*Bool)
	return ok && Bool{} == *r
}

func (*Bool) infer(Context) (Expression, error) { return &Type{}, nil }

func (*Bool) render() string { return "Bool" }

func (*Bool) renderBinary() binary { return binary{value: "Bool"} }

func (*Bool) renderCBOR() string { return fmt.Sprintf("%q", "Bool") }

func (bt *Bool) renderElm() (string, error) { return "Bool", nil }

func (bt *Bool) renderGo() (string, error) { return "bool", nil }

func (bt *Bool) renderHaskell() (string, error) { return "Bool", nil }

func (bt *Bool) renderJSON() (string, error) {
	return "", &JSONError{
		expression: bt,
		message:    "Cannot render type `Bool` to JSON",
	}
}

func (bt *Bool) renderJSONSchema() (string, error) {
	return fmt.Sprintf("{%q: %q}", "type", "boolean"), nil
}

func (bt *Bool) renderJavaScript() (string, error) {
	return "", &JavaScriptError{
		expression: bt,
		message:    "Cannot render type `Bool` to JavaScript",
	}
}

func (bt *Bool) renderPureScript() (string, error) { return "Boolean", nil }

func (bt *Bool) renderYAML() (string, error) {
	return "", &YAMLError{
		expression: bt,
		message:    "Cannot render type `Bool` to YAML",
	}
}

func (*Bool) shift(int, string, int) Expression { return &Bool{} }

func (*Bool) substitute(string, int, Expression) Expression {
	return &Bool{}
}

// BoolValue represents a Dhall BoolValue.
type BoolValue struct {
	Value bool
}

func (b *BoolValue) alphaNormalize() Expression { return b }

func (b *BoolValue) betaNormalize() Expression { return b }

func (b *BoolValue) equivalent(e Expression) bool {
	r, ok := e.betaNormalize().alphaNormalize().(*BoolValue)
	return ok && *b == *r
}

func (*BoolValue) infer(Context) (Expression, error) { return &Bool{}, nil }

func (b *BoolValue) render() string {
	if b.Value {
		return "True"
	}
	return "False"
}

func (b *BoolValue) renderBinary() binary { return binary{value: b.Value} }

func (b *BoolValue) renderCBOR() string { return fmt.Sprintf("%t", b.Value) }

func (b *BoolValue) renderElm() (string, error) {
	if b.Value {
		return "True", nil
	}
	return "False", nil
}

func (b *BoolValue) renderGo() (string, error) {
	if b.Value {
		return "true", nil
	}
	return "false", nil
}

func (b *BoolValue) renderHaskell() (string, error) {
	if b.Value {
		return "True", nil
	}
	return "False", nil
}

func (b *BoolValue) renderJSON() (string, error) {
	if b.Value {
		return "true", nil
	}
	return "false", nil
}

func (b *BoolValue) renderJSONSchema() (string, error) {
	return fmt.Sprintf("%t", b.Value), nil
}

func (b *BoolValue) renderJavaScript() (string, error) {
	if b.Value {
		return "true", nil
	}
	return "false", nil
}

func (b *BoolValue) renderPureScript() (string, error) {
	if b.Value {
		return "true", nil
	}
	return "false", nil
}

func (b *BoolValue) renderYAML() (string, error) {
	if b.Value {
		return "true", nil
	}
	return "false", nil
}

func (b *BoolValue) shift(int, string, int) Expression { return b }

func (b *BoolValue) substitute(string, int, Expression) Expression { return b }

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
	if l1.equivalent(r1) {
		return &BoolValue{Value: true}
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
		message:    "Cannot render `==` to JSON. Try normalizing first.",
	}
}

func (be *BoolEqual) renderJSONSchema() (string, error) {
	return "", &JSONSchemaError{
		expression: be,
		message:    "Cannot render `==` to JSONSchema. Try inferring the type.",
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
		message:    "Cannot render `==` to YAML. Try normalizing first.",
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

// BoolNotEqual represents equality of Dhall Bools.
type BoolNotEqual struct {
	Left  Expression
	Right Expression
}

func (be *BoolNotEqual) alphaNormalize() Expression {
	return &BoolNotEqual{
		Left:  be.Left.alphaNormalize(),
		Right: be.Right.alphaNormalize(),
	}
}

func (be *BoolNotEqual) betaNormalize() Expression {
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
	if l1.equivalent(r1) {
		return &BoolValue{Value: false}
	}
	return &BoolNotEqual{Left: l1, Right: r1}
}

func (be *BoolNotEqual) equivalent(e Expression) bool {
	l1 := be.betaNormalize().alphaNormalize()
	r1 := e.betaNormalize().alphaNormalize()
	l, lOk := l1.(*BoolNotEqual)
	r, rOk := r1.(*BoolNotEqual)
	return (lOk && rOk && *l == *r) || l1.equivalent(r1)
}

func (be *BoolNotEqual) infer(context Context) (Expression, error) {
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

func (be *BoolNotEqual) render() string {
	return fmt.Sprintf("%s != %s", be.Left.render(), be.Right.render())
}

func (be *BoolNotEqual) renderBinary() binary {
	l1 := be.Left.renderBinary()
	r1 := be.Right.renderBinary()
	return binary{
		value: [](interface{}){3, 3, l1.value, r1.value},
	}
}

func (be *BoolNotEqual) renderCBOR() string {
	l1 := be.Left.renderCBOR()
	r1 := be.Right.renderCBOR()
	return fmt.Sprintf("[3, 3, %s, %s]", l1, r1)
}

func (be *BoolNotEqual) renderElm() (string, error) {
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

func (be *BoolNotEqual) renderGo() (string, error) {
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

func (be *BoolNotEqual) renderHaskell() (string, error) {
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

func (be *BoolNotEqual) renderJSON() (string, error) {
	return "", &JSONError{
		expression: be,
		message:    "Cannot render `!=` to JSON. Try normalizing first.",
	}
}

func (be *BoolNotEqual) renderJSONSchema() (string, error) {
	return "", &JSONSchemaError{
		expression: be,
		message:    "Cannot render `!=` to JSONSchema. Try inferring the type.",
	}
}

func (be *BoolNotEqual) renderJavaScript() (string, error) {
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

func (be *BoolNotEqual) renderPureScript() (string, error) {
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

func (be *BoolNotEqual) renderYAML() (string, error) {
	return "", &YAMLError{
		expression: be,
		message:    "Cannot render `!=` to YAML. Try normalizing first.",
	}
}

func (be *BoolNotEqual) shift(d int, x string, m int) Expression {
	l1 := be.Left.shift(d, x, m)
	r1 := be.Right.shift(d, x, m)

	return &BoolNotEqual{Left: l1, Right: r1}
}

func (be *BoolNotEqual) substitute(x string, n int, e Expression) Expression {
	l1 := be.Left.substitute(x, n, e)
	r1 := be.Right.substitute(x, n, e)

	return &BoolNotEqual{Left: l1, Right: r1}
}

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
