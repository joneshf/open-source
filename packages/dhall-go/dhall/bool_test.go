package dhall

import (
	"fmt"
	"reflect"
	"testing"

	"github.com/leanovate/gopter"
	"github.com/leanovate/gopter/gen"
	"github.com/leanovate/gopter/prop"
	testifyAssert "github.com/stretchr/testify/assert"
	testifyRequire "github.com/stretchr/testify/require"
)

func genBool() gopter.Gen { return gen.Const(&Bool{}) }

func genBoolEqual() gopter.Gen {
	return genExpression().FlatMap(func(left interface{}) gopter.Gen {
		return genExpression().Map(func(right Expression) BoolEqual {
			return BoolEqual{Left: left.(Expression), Right: right}
		})
	}, reflect.TypeOf(BoolEqual{}))
}

func genBoolNotEqual() gopter.Gen {
	return genExpression().FlatMap(func(left interface{}) gopter.Gen {
		return genExpression().Map(func(right Expression) BoolNotEqual {
			return BoolNotEqual{Left: left.(Expression), Right: right}
		})
	}, reflect.TypeOf(BoolNotEqual{}))
}

func genBoolOr() gopter.Gen {
	return genExpression().FlatMap(func(left interface{}) gopter.Gen {
		return genExpression().Map(func(right Expression) BoolOr {
			return BoolOr{Left: left.(Expression), Right: right}
		})
	}, reflect.TypeOf(BoolOr{}))
}

func genBoolValue() gopter.Gen { return gen.OneGenOf(genFalse(), genTrue()) }

func genExpression() gopter.Gen {
	return gen.Weighted([]gen.WeightedGen{
		gen.WeightedGen{Weight: 1, Gen: genBool()},
		gen.WeightedGen{Weight: 1, Gen: genBoolValue()},
	})
}

func genFalse() gopter.Gen { return gen.Const(&BoolValue{Value: false}) }

func genTrue() gopter.Gen { return gen.Const(&BoolValue{Value: true}) }

func TestBool(t *testing.T) {
	assert := testifyAssert.New(t)
	properties := gopter.NewProperties(nil)
	require := testifyRequire.New(t)

	t.Run("alphaNormalize", func(t *testing.T) {
		require.Equal(&Bool{}, (&Bool{}).alphaNormalize())
	})

	t.Run("betaNormalize", func(t *testing.T) {
		require.Equal(&Bool{}, (&Bool{}).betaNormalize())
	})

	t.Run("equivalent", func(t *testing.T) {
		require.True(Equivalent(&Bool{}, &Bool{}))
	})

	t.Run("infer", func(t *testing.T) {
		actual, err := (&Bool{}).infer(emptyContext)
		require.NoError(err)
		require.Equal(&Type{}, actual)
	})

	t.Run("render", func(t *testing.T) {
		require.Equal("Bool", (&Bool{}).render())
	})

	t.Run("renderBinary", func(t *testing.T) {
		require.Equal(binary{value: "Bool"}, (&Bool{}).renderBinary())
	})

	t.Run("renderCBOR", func(t *testing.T) {
		require.Equal("\"Bool\"", (&Bool{}).renderCBOR())
	})

	t.Run("renderElm", func(t *testing.T) {
		actual, err := (&Bool{}).renderElm()
		require.NoError(err)
		require.Equal("Bool", actual)
	})

	t.Run("renderGo", func(t *testing.T) {
		actual, err := (&Bool{}).renderGo()
		require.NoError(err)
		require.Equal("bool", actual)
	})

	t.Run("renderHaskell", func(t *testing.T) {
		actual, err := (&Bool{}).renderHaskell()
		require.NoError(err)
		require.Equal("Bool", actual)
	})

	t.Run("renderJSON", func(t *testing.T) {
		unexpected, err := (&Bool{}).renderJSON()
		require.Error(err, "Did not expect to render to JSON: %s", unexpected)
	})

	t.Run("renderJSONSchema", func(t *testing.T) {
		actual, err := (&Bool{}).renderJSONSchema()
		require.NoError(err)
		require.Equal("{\"type\": \"boolean\"}", actual)
	})

	t.Run("renderJavaScript", func(t *testing.T) {
		unexpected, err := (&Bool{}).renderJavaScript()
		require.Error(
			err,
			"Did not expect to render to JavaScript: %s",
			unexpected,
		)
	})

	t.Run("renderPureScript", func(t *testing.T) {
		actual, err := (&Bool{}).renderPureScript()
		require.NoError(err)
		require.Equal("Boolean", actual)
	})

	t.Run("renderYAML", func(t *testing.T) {
		unexpected, err := (&Bool{}).renderYAML()
		require.Error(err, "Did not expect to render to YAML: %s", unexpected)
	})

	properties.Property("Shift has no effect", prop.ForAll(
		func(add int, variable string, index int) bool {
			return assert.Equal(&Bool{}, (&Bool{}).shift(add, variable, index))
		},
		gen.OneConstOf(-1, 1),
		gen.AnyString(),
		gen.Int(),
	))

	properties.Property("Substitution has no effect", prop.ForAll(
		func(variable string, index int, expression Expression) bool {
			return assert.Equal(
				&Bool{},
				(&Bool{}).substitute(variable, index, expression),
			)
		},
		gen.AnyString(),
		gen.Int(),
		genExpression(),
	))

	properties.TestingRun(t)
}

func TestBoolValue(t *testing.T) {
	assert := testifyAssert.New(t)
	properties := gopter.NewProperties(nil)
	require := testifyRequire.New(t)

	properties.Property("α-normalization has no effect", prop.ForAll(
		func(expression *BoolValue) bool {
			return assert.Equal(expression, expression.alphaNormalize())
		},
		genBoolValue(),
	))

	properties.Property("β-normalization has no effect", prop.ForAll(
		func(expression *BoolValue) bool {
			return assert.Equal(expression, expression.betaNormalize())
		},
		genBoolValue(),
	))

	t.Run("equivalent", func(t *testing.T) {
		require.Equal(&BoolValue{Value: false}, (&BoolValue{Value: false}))
		require.NotEqual(&BoolValue{Value: false}, (&BoolValue{Value: true}))
		require.NotEqual(&BoolValue{Value: true}, (&BoolValue{Value: false}))
		require.Equal(&BoolValue{Value: true}, (&BoolValue{Value: true}))
	})

	properties.Property("Inferred type is `Bool`", prop.ForAll(
		func(expression *BoolValue) bool {
			actual, err := expression.infer(emptyContext)
			return assert.NoError(err) && assert.Equal(&Bool{}, actual)
		},
		genBoolValue(),
	))

	t.Run("render", func(t *testing.T) {
		require.Equal("False", (&BoolValue{Value: false}).render())
		require.Equal("True", (&BoolValue{Value: true}).render())
	})

	t.Run("renderBinary", func(t *testing.T) {
		require.Equal(
			binary{value: false},
			(&BoolValue{Value: false}).renderBinary(),
		)
		require.Equal(
			binary{value: true},
			(&BoolValue{Value: true}).renderBinary(),
		)
	})

	t.Run("renderCBOR", func(t *testing.T) {
		require.Equal("false", (&BoolValue{Value: false}).renderCBOR())
		require.Equal("true", (&BoolValue{Value: true}).renderCBOR())
	})

	t.Run("renderElm", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).renderElm()
		require.NoError(err)
		require.Equal("False", actual)
		actual, err = (&BoolValue{Value: true}).renderElm()
		require.NoError(err)
		require.Equal("True", actual)
	})

	t.Run("renderGo", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).renderGo()
		require.NoError(err)
		require.Equal("false", actual)
		actual, err = (&BoolValue{Value: true}).renderGo()
		require.NoError(err)
		require.Equal("true", actual)
	})

	t.Run("renderHaskell", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).renderHaskell()
		require.NoError(err)
		require.Equal("False", actual)
		actual, err = (&BoolValue{Value: true}).renderHaskell()
		require.NoError(err)
		require.Equal("True", actual)
	})

	t.Run("renderJSON", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).renderJSON()
		require.NoError(err)
		require.Equal("false", actual)
		actual, err = (&BoolValue{Value: true}).renderJSON()
		require.NoError(err)
		require.Equal("true", actual)
	})

	t.Run("renderJSONSchema", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).renderJSONSchema()
		require.NoError(err)
		require.Equal("false", actual)
		actual, err = (&BoolValue{Value: true}).renderJSONSchema()
		require.NoError(err)
		require.Equal("true", actual)
	})

	t.Run("renderJavaScript", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).renderJavaScript()
		require.NoError(err)
		require.Equal("false", actual)
		actual, err = (&BoolValue{Value: true}).renderJavaScript()
		require.NoError(err)
		require.Equal("true", actual)
	})

	t.Run("renderPureScript", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).renderPureScript()
		require.NoError(err)
		require.Equal("false", actual)
		actual, err = (&BoolValue{Value: true}).renderPureScript()
		require.NoError(err)
		require.Equal("true", actual)
	})

	t.Run("renderYAML", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).renderYAML()
		require.NoError(err)
		require.Equal("false", actual)
		actual, err = (&BoolValue{Value: true}).renderYAML()
		require.NoError(err)
		require.Equal("true", actual)
	})

	properties.Property("Shift has no effect", prop.ForAll(
		func(expression *BoolValue, add int, variable string, index int) bool {
			return assert.Equal(
				expression,
				expression.shift(add, variable, index),
			)
		},
		genBoolValue(),
		gen.OneConstOf(-1, 1),
		gen.AnyString(),
		gen.Int(),
	))

	properties.Property("Substitution has no effect", prop.ForAll(
		func(b *BoolValue, variable string, index int, expression Expression) bool {
			return assert.Equal(b, b.substitute(variable, index, expression))
		},
		genBoolValue(),
		gen.AnyString(),
		gen.Int(),
		genExpression(),
	))

	properties.TestingRun(t)
}

func TestBoolEqual(t *testing.T) {
	assert := testifyAssert.New(t)
	properties := gopter.NewProperties(nil)
	require := testifyRequire.New(t)

	properties.Property("α-normalization has no effect", prop.ForAll(
		func(expression BoolEqual) bool {
			return assert.Equal(&expression, expression.alphaNormalize())
		},
		genBoolEqual(),
	))

	properties.Property("β-normalization works correctly", prop.ForAll(
		func(left Expression, right Expression) bool {
			if Equivalent(&BoolValue{Value: true}, left.betaNormalize()) {
				return assert.Equal(
					right.betaNormalize(),
					(&BoolEqual{Left: left, Right: right}).betaNormalize(),
				)
			}
			if Equivalent(&BoolValue{Value: true}, right.betaNormalize()) {
				return assert.Equal(
					left.betaNormalize(),
					(&BoolEqual{Left: left, Right: right}).betaNormalize(),
				)
			}
			if Equivalent(left, right) {
				return assert.Equal(
					&BoolValue{Value: true},
					(&BoolEqual{Left: left, Right: right}).betaNormalize(),
				)
			}
			return assert.Equal(
				&BoolEqual{
					Left:  left.betaNormalize(),
					Right: right.betaNormalize(),
				},
				(&BoolEqual{Left: left, Right: right}).betaNormalize(),
			)
		},
		genExpression(),
		genExpression(),
	))

	t.Run("equivalent", func(t *testing.T) {
		require.True(Equivalent(
			&BoolValue{Value: true},
			&BoolEqual{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: false},
			},
		))
		require.True(Equivalent(
			&BoolValue{Value: false},
			&BoolEqual{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: true},
			},
		))
		require.True(Equivalent(
			&BoolValue{Value: false},
			&BoolEqual{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: false},
			},
		))
		require.True(Equivalent(
			&BoolValue{Value: true},
			&BoolEqual{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: true},
			},
		))
	})

	properties.Property("Inference works correctly", prop.ForAll(
		func(left, right Expression) bool {
			actual, err := (&BoolEqual{Left: left, Right: right}).infer(
				emptyContext,
			)
			assert.NoError(err)
			return assert.Equal(&Bool{}, actual)
		},
		genExpression().SuchThat(func(e Expression) bool {
			actual, err := e.infer(emptyContext)
			return err == nil && reflect.DeepEqual(&Bool{}, actual)
		}),
		genExpression().SuchThat(func(e Expression) bool {
			actual, err := e.infer(emptyContext)
			return err == nil && reflect.DeepEqual(&Bool{}, actual)
		}),
	))

	properties.Property("render works correctly", prop.ForAll(
		func(left, right Expression) bool {
			out := fmt.Sprintf("%s == %s", left.render(), right.render())
			return assert.Equal(
				out,
				(&BoolEqual{Left: left, Right: right}).render(),
			)
		},
		genExpression(),
		genExpression(),
	))

	properties.Property("renderBinary works correctly", prop.ForAll(
		func(left, right Expression) bool {
			outLeft := left.renderBinary().value
			outRight := right.renderBinary().value
			out := binary{value: [](interface{}){3, 2, outLeft, outRight}}
			return assert.Equal(
				out,
				(&BoolEqual{Left: left, Right: right}).renderBinary(),
			)
		},
		genExpression(),
		genExpression(),
	))

	properties.Property("renderCBOR works correctly", prop.ForAll(
		func(left, right Expression) bool {
			out := fmt.Sprintf(
				"[3, 2, %s, %s]",
				left.renderCBOR(),
				right.renderCBOR(),
			)
			return assert.Equal(
				out,
				(&BoolEqual{Left: left, Right: right}).renderCBOR(),
			)
		},
		genExpression(),
		genExpression(),
	))

	properties.Property("renderElm works correctly", prop.ForAll(
		func(left, right Expression) bool {
			outLeft, err := left.renderElm()
			require.NoError(err)
			outRight, err := right.renderElm()
			require.NoError(err)
			expression := &BoolEqual{Left: left, Right: right}
			expected, err := expression.renderElm()
			require.NoError(err)
			actual := fmt.Sprintf("%s == %s", outLeft, outRight)
			return assert.Equal(actual, expected)
		},
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderElm()
			return err == nil
		}),
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderElm()
			return err == nil
		}),
	))

	properties.Property("renderGo works correctly", prop.ForAll(
		func(left, right Expression) bool {
			outLeft, err := left.renderGo()
			require.NoError(err)
			outRight, err := right.renderGo()
			require.NoError(err)
			expression := &BoolEqual{Left: left, Right: right}
			expected, err := expression.renderGo()
			require.NoError(err)
			actual := fmt.Sprintf("%s == %s", outLeft, outRight)
			return assert.Equal(actual, expected)
		},
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderGo()
			return err == nil
		}),
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderGo()
			return err == nil
		}),
	))

	properties.Property("renderHaskell works correctly", prop.ForAll(
		func(left, right Expression) bool {
			outLeft, err := left.renderHaskell()
			require.NoError(err)
			outRight, err := right.renderHaskell()
			require.NoError(err)
			expression := &BoolEqual{Left: left, Right: right}
			expected, err := expression.renderHaskell()
			require.NoError(err)
			actual := fmt.Sprintf("%s == %s", outLeft, outRight)
			return assert.Equal(actual, expected)
		},
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderHaskell()
			return err == nil
		}),
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderHaskell()
			return err == nil
		}),
	))

	properties.Property("renderJSON works correctly", prop.ForAll(
		func(left, right Expression) bool {
			expression := &BoolEqual{Left: left, Right: right}
			unexpected, err := expression.renderJSON()
			return assert.Error(
				err,
				"Did not expect to render to JSON: %s",
				unexpected,
			)
		},
		genExpression(),
		genExpression(),
	))

	properties.Property("renderJSONSchema works correctly", prop.ForAll(
		func(left, right Expression) bool {
			expression := &BoolEqual{Left: left, Right: right}
			unexpected, err := expression.renderJSONSchema()
			return assert.Error(
				err,
				"Did not expect to render to JSONSchema: %s",
				unexpected,
			)
		},
		genExpression(),
		genExpression(),
	))

	properties.Property("renderJavaScript works correctly", prop.ForAll(
		func(left, right Expression) bool {
			outLeft, err := left.renderJavaScript()
			require.NoError(err)
			outRight, err := right.renderJavaScript()
			require.NoError(err)
			expression := &BoolEqual{Left: left, Right: right}
			expected, err := expression.renderJavaScript()
			require.NoError(err)
			actual := fmt.Sprintf("%s === %s", outLeft, outRight)
			return assert.Equal(actual, expected)
		},
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderJavaScript()
			return err == nil
		}),
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderJavaScript()
			return err == nil
		}),
	))

	properties.Property("renderPureScript works correctly", prop.ForAll(
		func(left, right Expression) bool {
			outLeft, err := left.renderPureScript()
			require.NoError(err)
			outRight, err := right.renderPureScript()
			require.NoError(err)
			expression := &BoolEqual{Left: left, Right: right}
			expected, err := expression.renderPureScript()
			require.NoError(err)
			actual := fmt.Sprintf("%s == %s", outLeft, outRight)
			return assert.Equal(actual, expected)
		},
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderPureScript()
			return err == nil
		}),
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderPureScript()
			return err == nil
		}),
	))

	properties.Property("renderYAML works correctly", prop.ForAll(
		func(left, right Expression) bool {
			expression := &BoolEqual{Left: left, Right: right}
			unexpected, err := expression.renderYAML()
			return assert.Error(
				err,
				"Did not expect to render to YAML: %s",
				unexpected,
			)
		},
		genExpression(),
		genExpression(),
	))

	properties.Property("Shift descends into expressions", prop.ForAll(
		func(left, right Expression, add int, variable string, index int) bool {
			shiftLeft := left.shift(add, variable, index)
			shiftRight := right.shift(add, variable, index)
			expression := &BoolEqual{Left: left, Right: right}
			return assert.Equal(
				&BoolEqual{Left: shiftLeft, Right: shiftRight},
				expression.shift(add, variable, index),
			)
		},
		genExpression(),
		genExpression(),
		gen.OneConstOf(-1, 1),
		gen.AnyString(),
		gen.Int(),
	))

	properties.Property("Substitute descends into expressions", prop.ForAll(
		func(left, right Expression, variable string, index int, e Expression) bool {
			substituteLeft := left.substitute(variable, index, e)
			substituteRight := right.substitute(variable, index, e)
			expression := &BoolEqual{Left: left, Right: right}
			return assert.Equal(
				&BoolEqual{Left: substituteLeft, Right: substituteRight},
				expression.substitute(variable, index, e),
			)
		},
		genExpression(),
		genExpression(),
		gen.AnyString(),
		gen.Int(),
		genExpression(),
	))

	properties.TestingRun(t)
}

func TestBoolNotEqual(t *testing.T) {
	assert := testifyAssert.New(t)
	properties := gopter.NewProperties(nil)
	require := testifyRequire.New(t)

	properties.Property("α-normalization has no effect", prop.ForAll(
		func(expression BoolNotEqual) bool {
			return assert.Equal(&expression, expression.alphaNormalize())
		},
		genBoolNotEqual(),
	))

	properties.Property("β-normalization works correctly", prop.ForAll(
		func(left Expression, right Expression) bool {
			if Equivalent(&BoolValue{Value: false}, left.betaNormalize()) {
				return assert.Equal(
					right.betaNormalize(),
					(&BoolNotEqual{Left: left, Right: right}).betaNormalize(),
				)
			}
			if Equivalent(&BoolValue{Value: false}, right.betaNormalize()) {
				return assert.Equal(
					left.betaNormalize(),
					(&BoolNotEqual{Left: left, Right: right}).betaNormalize(),
				)
			}
			if Equivalent(left, right) {
				return assert.Equal(
					&BoolValue{Value: false},
					(&BoolNotEqual{Left: left, Right: right}).betaNormalize(),
				)
			}
			return assert.Equal(
				&BoolNotEqual{
					Left:  left.betaNormalize(),
					Right: right.betaNormalize(),
				},
				(&BoolNotEqual{Left: left, Right: right}).betaNormalize(),
			)
		},
		genExpression(),
		genExpression(),
	))

	t.Run("equivalent", func(t *testing.T) {
		require.True(Equivalent(
			&BoolValue{Value: false},
			&BoolNotEqual{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: false},
			},
		))
		require.True(Equivalent(
			&BoolValue{Value: true},
			&BoolNotEqual{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: true},
			},
		))
		require.True(Equivalent(
			&BoolValue{Value: true},
			&BoolNotEqual{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: false},
			},
		))
		require.True(Equivalent(
			&BoolValue{Value: false},
			&BoolNotEqual{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: true},
			},
		))
	})

	properties.Property("Inference works correctly", prop.ForAll(
		func(left, right Expression) bool {
			actual, err := (&BoolNotEqual{Left: left, Right: right}).infer(
				emptyContext,
			)
			assert.NoError(err)
			return assert.Equal(&Bool{}, actual)
		},
		genExpression().SuchThat(func(e Expression) bool {
			actual, err := e.infer(emptyContext)
			return err == nil && reflect.DeepEqual(&Bool{}, actual)
		}),
		genExpression().SuchThat(func(e Expression) bool {
			actual, err := e.infer(emptyContext)
			return err == nil && reflect.DeepEqual(&Bool{}, actual)
		}),
	))

	properties.Property("render works correctly", prop.ForAll(
		func(left, right Expression) bool {
			out := fmt.Sprintf("%s != %s", left.render(), right.render())
			return assert.Equal(
				out,
				(&BoolNotEqual{Left: left, Right: right}).render(),
			)
		},
		genExpression(),
		genExpression(),
	))

	properties.Property("renderBinary works correctly", prop.ForAll(
		func(left, right Expression) bool {
			outLeft := left.renderBinary().value
			outRight := right.renderBinary().value
			out := binary{value: [](interface{}){3, 3, outLeft, outRight}}
			return assert.Equal(
				out,
				(&BoolNotEqual{Left: left, Right: right}).renderBinary(),
			)
		},
		genExpression(),
		genExpression(),
	))

	properties.Property("renderCBOR works correctly", prop.ForAll(
		func(left, right Expression) bool {
			out := fmt.Sprintf(
				"[3, 3, %s, %s]",
				left.renderCBOR(),
				right.renderCBOR(),
			)
			return assert.Equal(
				out,
				(&BoolNotEqual{Left: left, Right: right}).renderCBOR(),
			)
		},
		genExpression(),
		genExpression(),
	))

	properties.Property("renderElm works correctly", prop.ForAll(
		func(left, right Expression) bool {
			outLeft, err := left.renderElm()
			require.NoError(err)
			outRight, err := right.renderElm()
			require.NoError(err)
			expression := &BoolNotEqual{Left: left, Right: right}
			expected, err := expression.renderElm()
			require.NoError(err)
			actual := fmt.Sprintf("%s /= %s", outLeft, outRight)
			return assert.Equal(actual, expected)
		},
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderElm()
			return err == nil
		}),
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderElm()
			return err == nil
		}),
	))

	properties.Property("renderGo works correctly", prop.ForAll(
		func(left, right Expression) bool {
			outLeft, err := left.renderGo()
			require.NoError(err)
			outRight, err := right.renderGo()
			require.NoError(err)
			expression := &BoolNotEqual{Left: left, Right: right}
			expected, err := expression.renderGo()
			require.NoError(err)
			actual := fmt.Sprintf("%s != %s", outLeft, outRight)
			return assert.Equal(actual, expected)
		},
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderGo()
			return err == nil
		}),
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderGo()
			return err == nil
		}),
	))

	properties.Property("renderHaskell works correctly", prop.ForAll(
		func(left, right Expression) bool {
			outLeft, err := left.renderHaskell()
			require.NoError(err)
			outRight, err := right.renderHaskell()
			require.NoError(err)
			expression := &BoolNotEqual{Left: left, Right: right}
			expected, err := expression.renderHaskell()
			require.NoError(err)
			actual := fmt.Sprintf("%s /= %s", outLeft, outRight)
			return assert.Equal(actual, expected)
		},
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderHaskell()
			return err == nil
		}),
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderHaskell()
			return err == nil
		}),
	))

	properties.Property("renderJSON works correctly", prop.ForAll(
		func(left, right Expression) bool {
			expression := &BoolNotEqual{Left: left, Right: right}
			unexpected, err := expression.renderJSON()
			return assert.Error(
				err,
				"Did not expect to render to JSON: %s",
				unexpected,
			)
		},
		genExpression(),
		genExpression(),
	))

	properties.Property("renderJSONSchema works correctly", prop.ForAll(
		func(left, right Expression) bool {
			expression := &BoolNotEqual{Left: left, Right: right}
			unexpected, err := expression.renderJSONSchema()
			return assert.Error(
				err,
				"Did not expect to render to JSONSchema: %s",
				unexpected,
			)
		},
		genExpression(),
		genExpression(),
	))

	properties.Property("renderJavaScript works correctly", prop.ForAll(
		func(left, right Expression) bool {
			outLeft, err := left.renderJavaScript()
			require.NoError(err)
			outRight, err := right.renderJavaScript()
			require.NoError(err)
			expression := &BoolNotEqual{Left: left, Right: right}
			expected, err := expression.renderJavaScript()
			require.NoError(err)
			actual := fmt.Sprintf("%s !== %s", outLeft, outRight)
			return assert.Equal(actual, expected)
		},
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderJavaScript()
			return err == nil
		}),
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderJavaScript()
			return err == nil
		}),
	))

	properties.Property("renderPureScript works correctly", prop.ForAll(
		func(left, right Expression) bool {
			outLeft, err := left.renderPureScript()
			require.NoError(err)
			outRight, err := right.renderPureScript()
			require.NoError(err)
			expression := &BoolNotEqual{Left: left, Right: right}
			expected, err := expression.renderPureScript()
			require.NoError(err)
			actual := fmt.Sprintf("%s /= %s", outLeft, outRight)
			return assert.Equal(actual, expected)
		},
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderPureScript()
			return err == nil
		}),
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderPureScript()
			return err == nil
		}),
	))

	properties.Property("renderYAML works correctly", prop.ForAll(
		func(left, right Expression) bool {
			expression := &BoolNotEqual{Left: left, Right: right}
			unexpected, err := expression.renderYAML()
			return assert.Error(
				err,
				"Did not expect to render to YAML: %s",
				unexpected,
			)
		},
		genExpression(),
		genExpression(),
	))

	properties.Property("Shift descends into expressions", prop.ForAll(
		func(left, right Expression, add int, variable string, index int) bool {
			shiftLeft := left.shift(add, variable, index)
			shiftRight := right.shift(add, variable, index)
			expression := &BoolNotEqual{Left: left, Right: right}
			return assert.Equal(
				&BoolNotEqual{Left: shiftLeft, Right: shiftRight},
				expression.shift(add, variable, index),
			)
		},
		genExpression(),
		genExpression(),
		gen.OneConstOf(-1, 1),
		gen.AnyString(),
		gen.Int(),
	))

	properties.Property("Substitute descends into expressions", prop.ForAll(
		func(left, right Expression, variable string, index int, e Expression) bool {
			substituteLeft := left.substitute(variable, index, e)
			substituteRight := right.substitute(variable, index, e)
			expression := &BoolNotEqual{Left: left, Right: right}
			return assert.Equal(
				&BoolNotEqual{Left: substituteLeft, Right: substituteRight},
				expression.substitute(variable, index, e),
			)
		},
		genExpression(),
		genExpression(),
		gen.AnyString(),
		gen.Int(),
		genExpression(),
	))

	properties.TestingRun(t)
}

func TestBoolOr(t *testing.T) {
	assert := testifyAssert.New(t)
	properties := gopter.NewProperties(nil)
	require := testifyRequire.New(t)

	properties.Property("α-normalization has no effect", prop.ForAll(
		func(expression BoolOr) bool {
			return assert.Equal(&expression, expression.alphaNormalize())
		},
		genBoolOr(),
	))

	properties.Property("β-normalization works correctly", prop.ForAll(
		func(left Expression, right Expression) bool {
			expression := &BoolOr{Left: left, Right: right}
			betaLeft := left.betaNormalize()
			betaRight := right.betaNormalize()
			actual := expression.betaNormalize()
			if reflect.DeepEqual(&BoolValue{Value: false}, betaLeft) {
				return assert.Equal(betaRight, actual)
			}
			if reflect.DeepEqual(&BoolValue{Value: false}, betaRight) {
				return assert.Equal(betaLeft, actual)
			}
			if reflect.DeepEqual(&BoolValue{Value: true}, betaLeft) {
				return assert.Equal(&BoolValue{Value: true}, actual)
			}
			if reflect.DeepEqual(&BoolValue{Value: true}, betaRight) {
				return assert.Equal(&BoolValue{Value: true}, actual)
			}
			if Equivalent(left, right) {
				return assert.Equal(left, actual)
			}
			return assert.Equal(expression, actual)
		},
		genExpression(),
		genExpression(),
	))

	t.Run("equivalent", func(t *testing.T) {
		require.True(Equivalent(
			&BoolValue{Value: false},
			&BoolOr{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: false},
			},
		))
		require.True(Equivalent(
			&BoolValue{Value: true},
			&BoolOr{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: true},
			},
		))
		require.True(Equivalent(
			&BoolValue{Value: true},
			&BoolOr{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: false},
			},
		))
		require.True(Equivalent(
			&BoolValue{Value: true},
			&BoolOr{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: true},
			},
		))
	})

	properties.Property("Inference works correctly", prop.ForAll(
		func(left, right Expression) bool {
			actual, err := (&BoolOr{Left: left, Right: right}).infer(
				emptyContext,
			)
			assert.NoError(err)
			return assert.Equal(&Bool{}, actual)
		},
		genExpression().SuchThat(func(e Expression) bool {
			actual, err := e.infer(emptyContext)
			return err == nil && reflect.DeepEqual(&Bool{}, actual)
		}),
		genExpression().SuchThat(func(e Expression) bool {
			actual, err := e.infer(emptyContext)
			return err == nil && reflect.DeepEqual(&Bool{}, actual)
		}),
	))

	properties.Property("render works correctly", prop.ForAll(
		func(left, right Expression) bool {
			out := fmt.Sprintf("%s || %s", left.render(), right.render())
			return assert.Equal(
				out,
				(&BoolOr{Left: left, Right: right}).render(),
			)
		},
		genExpression(),
		genExpression(),
	))

	properties.Property("renderBinary works correctly", prop.ForAll(
		func(left, right Expression) bool {
			outLeft := left.renderBinary().value
			outRight := right.renderBinary().value
			out := binary{value: [](interface{}){3, 0, outLeft, outRight}}
			return assert.Equal(
				out,
				(&BoolOr{Left: left, Right: right}).renderBinary(),
			)
		},
		genExpression(),
		genExpression(),
	))

	properties.Property("renderCBOR works correctly", prop.ForAll(
		func(left, right Expression) bool {
			out := fmt.Sprintf(
				"[3, 0, %s, %s]",
				left.renderCBOR(),
				right.renderCBOR(),
			)
			return assert.Equal(
				out,
				(&BoolOr{Left: left, Right: right}).renderCBOR(),
			)
		},
		genExpression(),
		genExpression(),
	))

	properties.Property("renderElm works correctly", prop.ForAll(
		func(left, right Expression) bool {
			outLeft, err := left.renderElm()
			require.NoError(err)
			outRight, err := right.renderElm()
			require.NoError(err)
			expression := &BoolOr{Left: left, Right: right}
			expected, err := expression.renderElm()
			require.NoError(err)
			actual := fmt.Sprintf("%s || %s", outLeft, outRight)
			return assert.Equal(actual, expected)
		},
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderElm()
			return err == nil
		}),
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderElm()
			return err == nil
		}),
	))

	properties.Property("renderGo works correctly", prop.ForAll(
		func(left, right Expression) bool {
			outLeft, err := left.renderGo()
			require.NoError(err)
			outRight, err := right.renderGo()
			require.NoError(err)
			expression := &BoolOr{Left: left, Right: right}
			expected, err := expression.renderGo()
			require.NoError(err)
			actual := fmt.Sprintf("%s || %s", outLeft, outRight)
			return assert.Equal(actual, expected)
		},
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderGo()
			return err == nil
		}),
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderGo()
			return err == nil
		}),
	))

	properties.Property("renderHaskell works correctly", prop.ForAll(
		func(left, right Expression) bool {
			outLeft, err := left.renderHaskell()
			require.NoError(err)
			outRight, err := right.renderHaskell()
			require.NoError(err)
			expression := &BoolOr{Left: left, Right: right}
			expected, err := expression.renderHaskell()
			require.NoError(err)
			actual := fmt.Sprintf("%s || %s", outLeft, outRight)
			return assert.Equal(actual, expected)
		},
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderHaskell()
			return err == nil
		}),
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderHaskell()
			return err == nil
		}),
	))

	properties.Property("renderJSON works correctly", prop.ForAll(
		func(left, right Expression) bool {
			expression := &BoolOr{Left: left, Right: right}
			unexpected, err := expression.renderJSON()
			return assert.Error(
				err,
				"Did not expect to render to JSON: %s",
				unexpected,
			)
		},
		genExpression(),
		genExpression(),
	))

	properties.Property("renderJSONSchema works correctly", prop.ForAll(
		func(left, right Expression) bool {
			expression := &BoolOr{Left: left, Right: right}
			unexpected, err := expression.renderJSONSchema()
			return assert.Error(
				err,
				"Did not expect to render to JSONSchema: %s",
				unexpected,
			)
		},
		genExpression(),
		genExpression(),
	))

	properties.Property("renderJavaScript works correctly", prop.ForAll(
		func(left, right Expression) bool {
			outLeft, err := left.renderJavaScript()
			require.NoError(err)
			outRight, err := right.renderJavaScript()
			require.NoError(err)
			expression := &BoolOr{Left: left, Right: right}
			expected, err := expression.renderJavaScript()
			require.NoError(err)
			actual := fmt.Sprintf("%s || %s", outLeft, outRight)
			return assert.Equal(actual, expected)
		},
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderJavaScript()
			return err == nil
		}),
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderJavaScript()
			return err == nil
		}),
	))

	properties.Property("renderPureScript works correctly", prop.ForAll(
		func(left, right Expression) bool {
			outLeft, err := left.renderPureScript()
			require.NoError(err)
			outRight, err := right.renderPureScript()
			require.NoError(err)
			expression := &BoolOr{Left: left, Right: right}
			expected, err := expression.renderPureScript()
			require.NoError(err)
			actual := fmt.Sprintf("%s || %s", outLeft, outRight)
			return assert.Equal(actual, expected)
		},
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderPureScript()
			return err == nil
		}),
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderPureScript()
			return err == nil
		}),
	))

	properties.Property("renderYAML works correctly", prop.ForAll(
		func(left, right Expression) bool {
			expression := &BoolOr{Left: left, Right: right}
			unexpected, err := expression.renderYAML()
			return assert.Error(
				err,
				"Did not expect to render to YAML: %s",
				unexpected,
			)
		},
		genExpression(),
		genExpression(),
	))

	properties.Property("Shift descends into expressions", prop.ForAll(
		func(left, right Expression, add int, variable string, index int) bool {
			shiftLeft := left.shift(add, variable, index)
			shiftRight := right.shift(add, variable, index)
			expression := &BoolOr{Left: left, Right: right}
			return assert.Equal(
				&BoolOr{Left: shiftLeft, Right: shiftRight},
				expression.shift(add, variable, index),
			)
		},
		genExpression(),
		genExpression(),
		gen.OneConstOf(-1, 1),
		gen.AnyString(),
		gen.Int(),
	))

	properties.Property("Substitute descends into expressions", prop.ForAll(
		func(left, right Expression, variable string, index int, e Expression) bool {
			substituteLeft := left.substitute(variable, index, e)
			substituteRight := right.substitute(variable, index, e)
			expression := &BoolOr{Left: left, Right: right}
			return assert.Equal(
				&BoolOr{Left: substituteLeft, Right: substituteRight},
				expression.substitute(variable, index, e),
			)
		},
		genExpression(),
		genExpression(),
		gen.AnyString(),
		gen.Int(),
		genExpression(),
	))

	properties.TestingRun(t)
}
