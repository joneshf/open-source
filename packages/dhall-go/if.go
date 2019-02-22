package dhall

import (
	"fmt"

	"github.com/joneshf/open-source/packages/go-pretty"
)

// If represents equality of Dhall Bools.
type If struct {
	Condition Expression
	Then      Expression
	Else      Expression
}

func (i *If) alphaNormalize() Expression {
	return &If{
		Condition: i.Condition.alphaNormalize(),
		Then:      i.Then.alphaNormalize(),
		Else:      i.Else.alphaNormalize(),
	}
}

func (i *If) betaNormalize() Expression {
	c1 := i.Condition.betaNormalize()
	c1Expression, ok := c1.(*BoolValue)
	if ok && (*c1Expression == BoolValue{Value: true}) {
		return i.Then.betaNormalize()
	}
	if ok && (*c1Expression == BoolValue{Value: false}) {
		return i.Else.betaNormalize()
	}
	t1 := i.Then.betaNormalize()
	t1Expression, t1Ok := t1.(*BoolValue)
	e1 := i.Else.betaNormalize()
	e1Expression, e1Ok := e1.(*BoolValue)
	if t1Ok &&
		e1Ok &&
		(*t1Expression == BoolValue{Value: true}) &&
		(*e1Expression == BoolValue{Value: false}) {
		return c1
	}
	if Equivalent(t1, e1) {
		return t1
	}
	return &If{Condition: c1, Then: t1, Else: e1}
}

func (i *If) infer(context Context) (Expression, error) {
	c, err := reduce(i.Condition, context)
	if err != nil {
		return nil, err
	}
	t, err := reduce(i.Then, context)
	if err != nil {
		return nil, err
	}
	tType, err := reduce(t, context)
	if err != nil {
		return nil, err
	}
	e, err := reduce(i.Else, context)
	if err != nil {
		return nil, err
	}
	eType, err := reduce(e, context)
	if err != nil {
		return nil, err
	}
	_, cOk := c.(*Bool)
	_, tOk := tType.(*Type)
	_, eOk := eType.(*Type)
	if cOk && tOk && eOk && Equivalent(t, e) {
		return t, nil
	}
	return nil, &TypeError{
		context: context,
		message: fmt.Sprintf(
			"Predicate must be type `Bool`, it was `%#v`."+
				" Then and else branches must be terms of the same type."+
				" Then was `%#v`. Else was `%#v`.",
			i.Condition,
			i.Then,
			i.Else,
		),
	}
}

func (i *If) render() pretty.Document {
	return pretty.Append(
		pretty.Spread(
			pretty.Text("if"),
			i.Condition.render(),
			pretty.Text("then"),
		),
		pretty.Nest(4, pretty.Append(pretty.Line, i.Then.render())),
		pretty.Line,
		pretty.Text("else"),
		pretty.Nest(4, pretty.Append(pretty.Line, i.Else.render())),
	)
}

func (i *If) renderBinary() binary {
	c := i.Condition.renderBinary()
	t := i.Then.renderBinary()
	e := i.Else.renderBinary()
	return binary{value: [](interface{}){14, c.value, t.value, e.value}}
}

func (i *If) renderCBOR() string {
	c := i.Condition.renderCBOR()
	t := i.Then.renderCBOR()
	e := i.Else.renderCBOR()
	return fmt.Sprintf("[14, %s, %s, %s]", c, t, e)
}

func (i *If) renderElm() (string, error) {
	c, err := i.Condition.renderElm()
	if err != nil {
		return "", err
	}
	t, err := i.Then.renderElm()
	if err != nil {
		return "", err
	}
	e, err := i.Else.renderElm()
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("if %s then %s else %s", c, t, e), nil
}

func (i *If) renderGo() (string, error) {
	return "", &JSONError{
		expression: i,
		message:    "Cannot render `if` to Go. Try normalizing first.",
	}
}

func (i *If) renderHaskell() (string, error) {
	c, err := i.Condition.renderHaskell()
	if err != nil {
		return "", err
	}
	t, err := i.Then.renderHaskell()
	if err != nil {
		return "", err
	}
	e, err := i.Else.renderHaskell()
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("if %s then %s else %s", c, t, e), nil
}

func (i *If) renderJSON() (string, error) {
	return "", &JSONError{
		expression: i,
		message:    "Cannot render `if` to JSON. Try normalizing first.",
	}
}

func (i *If) renderJSONSchema() (string, error) {
	return "", &JSONSchemaError{
		expression: i,
		message:    "Cannot render `if` to JSONSchema. Try inferring the type.",
	}
}

func (i *If) renderJavaScript() (string, error) {
	c, err := i.Condition.renderJavaScript()
	if err != nil {
		return "", err
	}
	t, err := i.Then.renderJavaScript()
	if err != nil {
		return "", err
	}
	e, err := i.Else.renderJavaScript()
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%s ? %s : %s", c, t, e), nil
}

func (i *If) renderPureScript() (string, error) {
	c, err := i.Condition.renderPureScript()
	if err != nil {
		return "", err
	}
	t, err := i.Then.renderPureScript()
	if err != nil {
		return "", err
	}
	e, err := i.Else.renderPureScript()
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("if %s then %s else %s", c, t, e), nil
}

func (i *If) renderYAML() (string, error) {
	return "", &YAMLError{
		expression: i,
		message:    "Cannot render `if` to YAML. Try normalizing first.",
	}
}

func (i *If) shift(d int, x string, m int) Expression {
	return &If{
		Condition: i.Condition.shift(d, x, m),
		Then:      i.Then.shift(d, x, m),
		Else:      i.Else.shift(d, x, m),
	}
}

func (i *If) substitute(x string, n int, e Expression) Expression {
	return &If{
		Condition: i.Condition.substitute(x, n, e),
		Then:      i.Then.substitute(x, n, e),
		Else:      i.Else.substitute(x, n, e),
	}
}
