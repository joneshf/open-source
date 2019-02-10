package main

import (
	"fmt"

	"github.com/ugorji/go/codec"
	"os"
)

type cbor struct {
	value interface{}
}

// Expression represents any valid Dhall expression.
type Expression interface {
	alphaNormalize() Expression
	betaNormalize() Expression
	encode() cbor
	infer(Context) (Expression, error)
	shift(int, string, int) Expression
	substitute(string, int, Expression) Expression
}

// DecodeError represents failures when attempting to decode to a Dhall expression.
type DecodeError struct {
	message string
	value   interface{}
}

func (e *DecodeError) Error() string {
	return fmt.Sprintf("%s. Raw value: %+v.", e.message, e.value)
}

func hydrate(raw interface{}) (Expression, error) {

	switch rawType := raw.(type) {
	case bool:
		return &Bool{value: raw.(bool)}, nil
	case string:
		switch rawType {
		case "Bool":
			return &BoolType{}, nil
		case "Kind":
			return &Kind{}, nil
		case "Sort":
			return &Sort{}, nil
		case "Type":
			return &Type{}, nil
		}
	case []interface{}:
		xs := raw.([]interface{})
		if len(xs) == 4 && xs[0].(uint64) == 3 && xs[1].(uint64) == 2 {
			left, err := hydrate(xs[2])
			if err != nil {
				return nil, err
			}
			right, err := hydrate(xs[3])
			if err != nil {
				return nil, err
			}
			return &BoolEqual{left: left, right: right}, nil
		}
	}

	return nil, &DecodeError{message: "Unhandled case", value: raw}
}

// Decode attempts to convert a binary encoding to a Dhall expression.
func Decode(handle *codec.CborHandle, in []byte) (Expression, error) {
	var raw interface{}

	decoder := codec.NewDecoderBytes(in, handle)
	if err := decoder.Decode(&raw); err != nil {
		return nil, err
	}

	return hydrate(raw)
}

// Encode attempts to convert an Expression to its binary encoding.
func Encode(handle *codec.CborHandle, expr Expression) ([]byte, error) {
	out := make([]byte, 0)
	encoder := codec.NewEncoderBytes(&out, handle)
	if err := encoder.Encode(expr.encode().value); err != nil {
		return nil, err
	}
	return out, nil
}

// Equivalent determines the following relationship between Expressions: l ≡ r.
func Equivalent(l0 Expression, r0 Expression) bool {
	l1 := l0.betaNormalize()
	r1 := r0.betaNormalize()
	le := l1.alphaNormalize()
	re := r1.alphaNormalize()
	return le == re
}

// Reduce is identical to Expression.infer
// except that it returns the inferred type in normal form.
func Reduce(a Expression, Γ Context) (Expression, error) {
	A0, err := a.infer(Γ)
	if err != nil {
		return nil, err
	}
	return A0.betaNormalize(), nil
}

type annotatedExpression struct {
	variable   string
	annotation Expression
}

// Context represents a Dhall Context.
type Context struct {
	value []annotatedExpression
}

func (c *Context) shift(d int, x string, m int) Context {
	if len(c.value) == 0 {
		return *c
	}
	oldHead, tail := c.value[0], c.value[1:]
	Γ0 := &Context{value: tail}
	Γ1 := Γ0.shift(d, x, m)
	T1 := oldHead.annotation.shift(d, x, m)
	newHead := annotatedExpression{variable: oldHead.variable, annotation: T1}

	return Context{value: append([]annotatedExpression{newHead}, Γ1.value...)}
}

// TypeError represents failures when inferring the type of an expression.
type TypeError struct {
	context Context
	message string
}

func (e *TypeError) Error() string {
	return fmt.Sprintf("%s. Context: %s.", e.message, e.context)
}

// FunctionCheckError represents failures when determining function types.
type FunctionCheckError struct {
	constant Constant
	message  string
}

func (e *FunctionCheckError) Error() string {
	return fmt.Sprintf("%s. Constant: %s.", e.message, e.constant)
}

// Constant represents Dhall Constants
type Constant interface {
	functionCheck(Constant) (Constant, error)
}

// Kind represents the type of Dhall types.
type Kind struct{}

func (*Kind) alphaNormalize() Expression { return &Kind{} }

func (*Kind) betaNormalize() Expression { return &Kind{} }

func (*Kind) encode() cbor { return cbor{value: "Kind"} }

func (*Kind) functionCheck(c Constant) (Constant, error) {
	switch c.(type) {
	case *Kind:
		return &Kind{}, nil
	case *Sort:
		return nil, &FunctionCheckError{
			constant: c,
			message:  "Dependent types not supported for `Kind`",
		}
	case *Type:
		return &Type{}, nil
	default:
		return nil, &FunctionCheckError{
			constant: c,
			message:  "Unhandled function check case for `Kind`",
		}
	}
}

func (*Kind) infer(Context) (Expression, error) { return &Sort{}, nil }

func (*Kind) shift(int, string, int) Expression { return &Kind{} }

func (*Kind) substitute(string, int, Expression) Expression { return &Kind{} }

// Sort represents the type of Dhall kinds.
type Sort struct{}

func (*Sort) alphaNormalize() Expression { return &Sort{} }

func (*Sort) betaNormalize() Expression { return &Sort{} }

func (*Sort) encode() cbor { return cbor{value: "Sort"} }

func (*Sort) functionCheck(c Constant) (Constant, error) {
	switch c.(type) {
	case *Kind:
		return &Kind{}, nil
	case *Sort:
		return &Sort{}, nil
	case *Type:
		return &Type{}, nil
	default:
		return nil, &FunctionCheckError{
			constant: c,
			message:  "Unhandled function check case for `Sort`",
		}
	}
}

func (*Sort) infer(context Context) (Expression, error) {
	return nil, &TypeError{
		context: context,
		message: "Cannot infer type of `Sort`",
	}
}

func (*Sort) shift(int, string, int) Expression { return &Sort{} }

func (*Sort) substitute(string, int, Expression) Expression { return &Sort{} }

// Type represents the type of Dhall terms.
type Type struct{}

func (*Type) alphaNormalize() Expression { return &Type{} }

func (*Type) betaNormalize() Expression { return &Type{} }

func (*Type) encode() cbor { return cbor{value: "Type"} }

func (*Type) functionCheck(c Constant) (Constant, error) {
	switch c.(type) {
	case *Kind:
		return nil, &FunctionCheckError{
			constant: c,
			message:  "Dependent types not supported for `Type`",
		}
	case *Sort:
		return nil, &FunctionCheckError{
			constant: c,
			message:  "Dependent types not supported for `Type`",
		}
	case *Type:
		return &Type{}, nil
	default:
		return nil, &FunctionCheckError{
			constant: c,
			message:  "Unhandled function check case for `Type`",
		}
	}
}

func (*Type) infer(Context) (Expression, error) { return &Kind{}, nil }

func (*Type) shift(int, string, int) Expression { return &Type{} }

func (*Type) substitute(string, int, Expression) Expression { return &Type{} }

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
	value bool
}

func (b *Bool) alphaNormalize() Expression { return b }

func (b *Bool) betaNormalize() Expression { return b }

func (b *Bool) encode() cbor { return cbor{value: b.value} }

func (*Bool) infer(Context) (Expression, error) { return &BoolType{}, nil }

func (b *Bool) shift(int, string, int) Expression { return b }

func (b *Bool) substitute(string, int, Expression) Expression { return b }

// BoolEqual represents equality of Dhall Bools.
type BoolEqual struct {
	left  Expression
	right Expression
}

func (be *BoolEqual) alphaNormalize() Expression {
	return &BoolEqual{
		left:  be.left.alphaNormalize(),
		right: be.right.alphaNormalize(),
	}
}

func (be *BoolEqual) betaNormalize() Expression {
	l1 := be.left.betaNormalize()
	if (l1 == &Bool{value: true}) {
		return be.right.betaNormalize()
	}
	r1 := be.right.betaNormalize()
	if (r1 == &Bool{value: true}) {
		return l1
	}
	if Equivalent(l1, r1) {
		return &Bool{value: true}
	}
	return &BoolEqual{left: l1, right: r1}
}

func (be *BoolEqual) encode() cbor {
	l1 := be.left.encode()
	r1 := be.right.encode()
	return cbor{value: [](interface{}){3, 2, l1.value, r1.value}}
}

func (be *BoolEqual) infer(context Context) (Expression, error) {
	l, err := Reduce(be.left, context)
	if err != nil {
		return nil, err
	}
	r, err := Reduce(be.right, context)
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
	l1 := be.left.shift(d, x, m)
	r1 := be.right.shift(d, x, m)

	return &BoolEqual{left: l1, right: r1}
}

func (be *BoolEqual) substitute(x string, n int, e Expression) Expression {
	l1 := be.left.substitute(x, n, e)
	r1 := be.right.substitute(x, n, e)

	return &BoolEqual{left: l1, right: r1}
}

func main() {
	handle := &codec.CborHandle{}
	t := &Bool{value: true}
	f := &Bool{value: false}
	expr := &BoolEqual{left: t, right: f}

	encoded, err := Encode(handle, expr)
	if err != nil {
		fmt.Printf("Failed to encode %+v: %+v\n", expr, err)
		os.Exit(1)
	}
	fmt.Printf("Successfully encoded %+v: %+X\n", expr, encoded)

	decoded, err := Decode(handle, encoded)
	if err != nil {
		fmt.Printf("Failed to decode %+v: %+v\n", expr, err)
		os.Exit(1)
	}
	fmt.Printf("Successfully decoded %+v: %+v\n", expr, decoded)
}
