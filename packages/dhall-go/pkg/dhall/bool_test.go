package dhall

import (
	"testing"

	"github.com/leanovate/gopter"
	"github.com/leanovate/gopter/gen"
	"github.com/leanovate/gopter/prop"
	testifyAssert "github.com/stretchr/testify/assert"
	testifyRequire "github.com/stretchr/testify/require"
)

func genBool() gopter.Gen { return gen.Const(&Bool{}) }

func genBoolValue() gopter.Gen { return gen.OneGenOf(genFalse(), genTrue()) }

func genExpression() gopter.Gen {
	return gen.Weighted([]gen.WeightedGen{
		gen.WeightedGen{Weight: 1, Gen: genBool()},
		gen.WeightedGen{Weight: 5, Gen: genBoolValue()},
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
