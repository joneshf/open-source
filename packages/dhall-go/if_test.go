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

func genIf(size int) gopter.Gen {
	return gen.StructPtr(reflect.TypeOf(&If{}), map[string]gopter.Gen{
		"Condition": genSizedExpression(size / 2),
		"Then":      genSizedExpression(size / 2),
		"Else":      genSizedExpression(size / 2),
	}).Map(func(e *If) Expression { return e })
}

func TestIf(t *testing.T) {
	assert := testifyAssert.New(t)
	properties := gopter.NewProperties(nil)
	require := testifyRequire.New(t)

	properties.Property("α-normalization has no effect", prop.ForAll(
		func(expression *If) bool {
			return assert.Equal(expression, expression.alphaNormalize())
		},
		sized(genIf),
	))

	properties.Property("β-normalization works correctly", prop.ForAll(
		func(c, t, e Expression) bool {
			expression := &If{Condition: c, Then: t, Else: e}
			betaC := c.betaNormalize()
			betaT := t.betaNormalize()
			betaE := e.betaNormalize()
			actual := expression.betaNormalize()
			if reflect.DeepEqual(&BoolValue{Value: true}, betaC) {
				return assert.Equal(betaT, actual)
			}
			if reflect.DeepEqual(&BoolValue{Value: false}, betaC) {
				return assert.Equal(betaE, actual)
			}
			if reflect.DeepEqual(&BoolValue{Value: true}, betaT) &&
				reflect.DeepEqual(&BoolValue{Value: false}, betaE) {
				return assert.Equal(betaC, actual)
			}
			if Equivalent(t, e) {
				return assert.Equal(betaT, actual)
			}
			return assert.Equal(
				&If{Condition: betaC, Then: betaT, Else: betaE},
				actual,
			)
		},
		genExpression(),
		genExpression(),
		genExpression(),
	))

	properties.Property("Inference works correctly", prop.ForAll(
		func(c, tE Expression) bool {
			actual, err := (&If{Condition: c, Then: tE, Else: tE}).infer(
				emptyContext,
			)
			require.NoError(err)
			expected, err := tE.infer(emptyContext)
			require.NoError(err)
			return assert.Equal(expected, actual)
		},
		genExpression().SuchThat(func(e Expression) bool {
			actual, err := e.infer(emptyContext)
			return err == nil && reflect.DeepEqual(&Bool{}, actual)
		}),
		genExpression().SuchThat(func(e Expression) bool {
			eType, err := e.infer(emptyContext)
			if err != nil {
				return false
			}
			eKind, err := eType.infer(emptyContext)
			if err != nil {
				return false
			}
			_, ok := eKind.(*Type)
			return ok
		}),
	))

	properties.Property("render works correctly", prop.ForAll(
		func(c, t, e Expression) bool {
			out := fmt.Sprintf(
				"if %s then\n    %s\nelse\n    %s",
				pretty.Render(0, c.render()),
				pretty.Render(0, pretty.Nest(4, t.render())),
				pretty.Render(0, pretty.Nest(4, e.render())),
			)
			return assert.Equal(
				out,
				pretty.Render(0, (&If{Condition: c, Then: t, Else: e}).render()),
			)
		},
		genExpression(),
		genExpression(),
		genExpression(),
	))

	properties.Property("renderBinary works correctly", prop.ForAll(
		func(c, t, e Expression) bool {
			out := binary{value: [](interface{}){
				14,
				c.renderBinary().value,
				t.renderBinary().value,
				e.renderBinary().value,
			}}
			return assert.Equal(
				out,
				(&If{Condition: c, Then: t, Else: e}).renderBinary(),
			)
		},
		genExpression(),
		genExpression(),
		genExpression(),
	))

	properties.Property("renderCBOR works correctly", prop.ForAll(
		func(c, t, e Expression) bool {
			out := fmt.Sprintf(
				"[\n    14,\n    %s,\n    %s,\n    %s\n]",
				pretty.Render(0, pretty.Nest(4, c.renderCBOR())),
				pretty.Render(0, pretty.Nest(4, t.renderCBOR())),
				pretty.Render(0, pretty.Nest(4, e.renderCBOR())),
			)
			return assert.Equal(
				out,
				pretty.Render(
					0,
					(&If{Condition: c, Then: t, Else: e}).renderCBOR(),
				),
			)
		},
		genExpression(),
		genExpression(),
		genExpression(),
	))

	properties.Property("renderElm works correctly", prop.ForAll(
		func(c, t, e Expression) bool {
			c1, err := c.renderElm()
			require.NoError(err)
			t1, err := t.renderElm()
			require.NoError(err)
			e1, err := e.renderElm()
			require.NoError(err)
			out := fmt.Sprintf("if %s then %s else %s", c1, t1, e1)
			actual, err := (&If{Condition: c, Then: t, Else: e}).renderElm()
			require.NoError(err)
			return assert.Equal(out, actual)
		},
		genExpression(),
		genExpression(),
		genExpression(),
	))

	properties.Property("renderGo works correctly", prop.ForAll(
		func(c, t, e Expression) bool {
			expression := &If{Condition: c, Then: t, Else: e}
			unexpected, err := expression.renderGo()
			return assert.Error(
				err,
				"Did not expect to render to Go: %s",
				unexpected,
			)
		},
		genExpression(),
		genExpression(),
		genExpression(),
	))

	properties.Property("renderHaskell works correctly", prop.ForAll(
		func(c, t, e Expression) bool {
			c1, err := c.renderHaskell()
			require.NoError(err)
			t1, err := t.renderHaskell()
			require.NoError(err)
			e1, err := e.renderHaskell()
			require.NoError(err)
			out := fmt.Sprintf("if %s then %s else %s", c1, t1, e1)
			actual, err := (&If{Condition: c, Then: t, Else: e}).renderHaskell()
			require.NoError(err)
			return assert.Equal(out, actual)
		},
		genExpression(),
		genExpression(),
		genExpression(),
	))

	properties.Property("renderJSON works correctly", prop.ForAll(
		func(c, t, e Expression) bool {
			expression := &If{Condition: c, Then: t, Else: e}
			unexpected, err := expression.renderJSON()
			return assert.Error(
				err,
				"Did not expect to render to JSON: %s",
				unexpected,
			)
		},
		genExpression(),
		genExpression(),
		genExpression(),
	))

	properties.Property("renderJSONSchema works correctly", prop.ForAll(
		func(c, t, e Expression) bool {
			expression := &If{Condition: c, Then: t, Else: e}
			unexpected, err := expression.renderJSONSchema()
			return assert.Error(
				err,
				"Did not expect to render to JSONSchema: %s",
				unexpected,
			)
		},
		genExpression(),
		genExpression(),
		genExpression(),
	))

	properties.Property("renderJavaScript works correctly", prop.ForAll(
		func(c, t, e Expression) bool {
			c1, err := c.renderJavaScript()
			require.NoError(err)
			t1, err := t.renderJavaScript()
			require.NoError(err)
			e1, err := e.renderJavaScript()
			require.NoError(err)
			out := fmt.Sprintf("%s ? %s : %s", c1, t1, e1)
			actual, err := (&If{Condition: c, Then: t, Else: e}).renderJavaScript()
			require.NoError(err)
			return assert.Equal(out, actual)
		},
		genExpression().SuchThat(func(e Expression) bool {
			_, err := e.renderJavaScript()
			return err == nil
		}),
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
		func(c, t, e Expression) bool {
			c1, err := c.renderPureScript()
			require.NoError(err)
			t1, err := t.renderPureScript()
			require.NoError(err)
			e1, err := e.renderPureScript()
			require.NoError(err)
			out := fmt.Sprintf("if %s then %s else %s", c1, t1, e1)
			actual, err := (&If{Condition: c, Then: t, Else: e}).renderPureScript()
			require.NoError(err)
			return assert.Equal(out, actual)
		},
		genExpression(),
		genExpression(),
		genExpression(),
	))

	properties.Property("renderYAML works correctly", prop.ForAll(
		func(c, t, e Expression) bool {
			expression := &If{Condition: c, Then: t, Else: e}
			unexpected, err := expression.renderYAML()
			return assert.Error(
				err,
				"Did not expect to render to YAML: %s",
				unexpected,
			)
		},
		genExpression(),
		genExpression(),
		genExpression(),
	))

	properties.Property("Shift descends into expressions", prop.ForAll(
		func(c, t, e Expression, add int, variable string, index int) bool {
			shiftC := c.shift(add, variable, index)
			shiftT := t.shift(add, variable, index)
			shiftE := e.shift(add, variable, index)
			expression := &If{Condition: c, Then: t, Else: e}
			return assert.Equal(
				&If{Condition: shiftC, Then: shiftT, Else: shiftE},
				expression.shift(add, variable, index),
			)
		},
		genExpression(),
		genExpression(),
		genExpression(),
		gen.OneConstOf(-1, 1),
		gen.AnyString(),
		gen.Int(),
	))

	properties.Property("Substitute descends into expressions", prop.ForAll(
		func(c, t, e Expression, variable string, index int, e1 Expression) bool {
			substituteC := c.substitute(variable, index, e1)
			substituteT := t.substitute(variable, index, e1)
			substituteE := e.substitute(variable, index, e1)
			expression := &If{Condition: c, Then: t, Else: e}
			return assert.Equal(
				&If{Condition: substituteC, Then: substituteT, Else: substituteE},
				expression.substitute(variable, index, e1),
			)
		},
		genExpression(),
		genExpression(),
		genExpression(),
		gen.AnyString(),
		gen.Int(),
		genExpression(),
	))

	properties.TestingRun(t)
}
