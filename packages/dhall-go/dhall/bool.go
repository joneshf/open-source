package dhall

import (
	"fmt"
)

// BoolType represents the type of Dhall Bools.
type BoolType struct{}

func (*BoolType) alphaNormalize() Expression { return &BoolType{} }

func (*BoolType) betaNormalize() Expression { return &BoolType{} }

func (*BoolType) encode() cbor { return cbor{value: "Bool"} }

func (*BoolType) equivalent(e Expression) bool {
	r, ok := e.betaNormalize().alphaNormalize().(*BoolType)
	return ok && BoolType{} == *r
}

func (*BoolType) infer(Context) (Expression, error) { return &Type{}, nil }

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

func (b *Bool) encode() cbor { return cbor{value: b.Value} }

func (b *Bool) equivalent(e Expression) bool {
	r, ok := e.betaNormalize().alphaNormalize().(*Bool)
	return ok && *b == *r
}

func (*Bool) infer(Context) (Expression, error) { return &BoolType{}, nil }

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

func (be *BoolEqual) encode() cbor {
	l1 := be.Left.encode()
	r1 := be.Right.encode()
	return cbor{value: [](interface{}){3, 2, l1.value, r1.value}}
}

func (be *BoolEqual) equivalent(e Expression) bool {
	l1 := be.betaNormalize().alphaNormalize()
	r1 := e.betaNormalize().alphaNormalize()
	l, lOk := l1.(*BoolEqual)
	r, rOk := r1.(*BoolEqual)
	return (lOk && rOk && *l == *r) || l1.equivalent(r1)
}

func (be *BoolEqual) infer(context Context) (Expression, error) {
	l, err := Reduce(be.Left, context)
	if err != nil {
		return nil, err
	}
	r, err := Reduce(be.Right, context)
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
