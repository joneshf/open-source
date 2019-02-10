package dhall

// BoolType represents the type of Dhall Bools.
type BoolType struct{}

func (*BoolType) alphaNormalize() Expression { return &BoolType{} }

func (*BoolType) betaNormalize() Expression { return &BoolType{} }

func (*BoolType) encode() cbor { return cbor{value: "Bool"} }

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
	if (l1 == &Bool{Value: true}) {
		return be.Right.betaNormalize()
	}
	r1 := be.Right.betaNormalize()
	if (r1 == &Bool{Value: true}) {
		return l1
	}
	if Equivalent(l1, r1) {
		return &Bool{Value: true}
	}
	return &BoolEqual{Left: l1, Right: r1}
}

func (be *BoolEqual) encode() cbor {
	l1 := be.Left.encode()
	r1 := be.Right.encode()
	return cbor{value: [](interface{}){3, 2, l1.value, r1.value}}
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
	if (l == &Bool{} && r == &Bool{}) {
		return &Bool{}, nil
	}
	return nil, &TypeError{
		context: context,
		message: "Both arguments to `==` must have type `Bool`",
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
