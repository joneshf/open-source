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

	"github.com/joneshf/open-source/packages/go-pretty"
)

func genOr(size int) gopter.Gen {
	return gen.StructPtr(reflect.TypeOf(&Or{}), map[string]gopter.Gen{
		"Left":  genSizedExpression(size / 2),
		"Right": genSizedExpression(size / 2),
	}).Map(func(e *Or) Expression { return e })
}

func TestOr(t *testing.T) {
	assert := testifyAssert.New(t)
	properties := gopter.NewProperties(nil)
	require := testifyRequire.New(t)

	properties.Property("α-normalization has no effect", prop.ForAll(
		func(expression *Or) bool {
			return assert.Equal(expression, expression.alphaNormalize())
		},
		sized(genOr),
	))

	properties.Property("β-normalization works correctly", prop.ForAll(
		func(left, right Expression) bool {
			expression := &Or{Left: left, Right: right}
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
				return assert.Equal(betaLeft, actual)
			}
			return assert.Equal(&Or{Left: betaLeft, Right: betaRight}, actual)
		},
		genExpression(),
		genExpression(),
	))

	properties.Property("Inference works correctly", prop.ForAll(
		func(left, right Expression) bool {
			actual, err := (&Or{Left: left, Right: right}).infer(
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
			out := fmt.Sprintf(
				"%s || %s",
				pretty.Render(0, left.render()),
				pretty.Render(0, right.render()),
			)
			return assert.Equal(
				out,
				pretty.Render(0, (&Or{Left: left, Right: right}).render()),
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
				(&Or{Left: left, Right: right}).renderBinary(),
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
				(&Or{Left: left, Right: right}).renderCBOR(),
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
			expression := &Or{Left: left, Right: right}
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
			expression := &Or{Left: left, Right: right}
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
			expression := &Or{Left: left, Right: right}
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
			expression := &Or{Left: left, Right: right}
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
			expression := &Or{Left: left, Right: right}
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
			expression := &Or{Left: left, Right: right}
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
			expression := &Or{Left: left, Right: right}
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
			expression := &Or{Left: left, Right: right}
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
			expression := &Or{Left: left, Right: right}
			return assert.Equal(
				&Or{Left: shiftLeft, Right: shiftRight},
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
			expression := &Or{Left: left, Right: right}
			return assert.Equal(
				&Or{Left: substituteLeft, Right: substituteRight},
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
