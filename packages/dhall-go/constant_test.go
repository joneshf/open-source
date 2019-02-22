package dhall

import (
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/joneshf/open-source/packages/go-pretty"
)

func TestKind(t *testing.T) {
	assert := require.New(t)

	t.Run("alphaNormalize", func(t *testing.T) {
		assert.Equal(&Kind{}, (&Kind{}).alphaNormalize())
	})

	t.Run("betaNormalize", func(t *testing.T) {
		assert.Equal(&Kind{}, (&Kind{}).betaNormalize())
	})

	t.Run("functionCheck", func(t *testing.T) {
		actual, err := (&Kind{}).functionCheck(&Kind{})
		assert.NoError(err)
		assert.Equal(&Kind{}, actual)
		unexpected, err := (&Kind{}).functionCheck(&Sort{})
		assert.Error(
			err,
			"Did not expect to allow dependent types: %#v",
			unexpected,
		)
		actual, err = (&Kind{}).functionCheck(&Type{})
		assert.NoError(err)
		assert.Equal(&Type{}, actual)
	})

	t.Run("infer", func(t *testing.T) {
		actual, err := (&Kind{}).infer(emptyContext)
		assert.NoError(err)
		assert.Equal(&Sort{}, actual)
	})

	t.Run("render", func(t *testing.T) {
		assert.Equal("Kind", pretty.Render(0, (&Kind{}).render()))
	})

	t.Run("renderBinary", func(t *testing.T) {
		assert.Equal(binary{value: "Kind"}, (&Kind{}).renderBinary())
	})

	t.Run("renderCBOR", func(t *testing.T) {
		assert.Equal("\"Kind\"", (&Kind{}).renderCBOR())
	})

	t.Run("renderElm", func(t *testing.T) {
		unexpected, err := (&Kind{}).renderElm()
		assert.Error(err, "Did not expect to render to Elm: %s", unexpected)
	})

	t.Run("renderGo", func(t *testing.T) {
		unexpected, err := (&Kind{}).renderGo()
		assert.Error(err, "Did not expect to render to Go: %s", unexpected)
	})

	t.Run("renderHaskell", func(t *testing.T) {
		unexpected, err := (&Kind{}).renderHaskell()
		assert.Error(err, "Did not expect to render to Haskell: %s", unexpected)
	})

	t.Run("renderJSON", func(t *testing.T) {
		unexpected, err := (&Kind{}).renderJSON()
		assert.Error(err, "Did not expect to render to JSON: %s", unexpected)
	})

	t.Run("renderJSONSchemaSchema", func(t *testing.T) {
		unexpected, err := (&Kind{}).renderJSONSchema()
		assert.Error(
			err,
			"Did not expect to render to JSONSchema: %s",
			unexpected,
		)
	})

	t.Run("renderJavaScript", func(t *testing.T) {
		unexpected, err := (&Kind{}).renderJavaScript()
		assert.Error(
			err,
			"Did not expect to render to JavaScript: %s",
			unexpected,
		)
	})

	t.Run("renderPureScript", func(t *testing.T) {
		unexpected, err := (&Kind{}).renderPureScript()
		assert.Error(
			err,
			"Did not expect to render to PureScript: %s",
			unexpected,
		)
	})

	t.Run("renderYAML", func(t *testing.T) {
		unexpected, err := (&Kind{}).renderYAML()
		assert.Error(err, "Did not expect to render to YAML: %s", unexpected)
	})

	t.Run("shift", func(t *testing.T) {
		assert.Equal(&Kind{}, (&Kind{}).shift(0, "", 0))
	})

	t.Run("substitute", func(t *testing.T) {
		assert.Equal(
			&Kind{},
			(&Kind{}).substitute("", 0, &BoolValue{Value: true}),
		)
	})
}

func TestSort(t *testing.T) {
	assert := require.New(t)

	t.Run("alphaNormalize", func(t *testing.T) {
		assert.Equal(&Sort{}, (&Sort{}).alphaNormalize())
	})

	t.Run("betaNormalize", func(t *testing.T) {
		assert.Equal(&Sort{}, (&Sort{}).betaNormalize())
	})

	t.Run("functionCheck", func(t *testing.T) {
		actual, err := (&Sort{}).functionCheck(&Kind{})
		assert.NoError(err)
		assert.Equal(&Kind{}, actual)
		actual, err = (&Sort{}).functionCheck(&Sort{})
		assert.NoError(err)
		assert.Equal(&Sort{}, actual)
		actual, err = (&Sort{}).functionCheck(&Type{})
		assert.NoError(err)
		assert.Equal(&Type{}, actual)
	})

	t.Run("infer", func(t *testing.T) {
		unexpected, err := (&Sort{}).infer(emptyContext)
		assert.Error(
			err,
			"Did not expect to infer the type of `Sort`: %#v",
			unexpected,
		)
	})

	t.Run("render", func(t *testing.T) {
		assert.Equal("Sort", pretty.Render(0, (&Sort{}).render()))
	})

	t.Run("renderBinary", func(t *testing.T) {
		assert.Equal(binary{value: "Sort"}, (&Sort{}).renderBinary())
	})

	t.Run("renderCBOR", func(t *testing.T) {
		assert.Equal("\"Sort\"", (&Sort{}).renderCBOR())
	})

	t.Run("renderElm", func(t *testing.T) {
		unexpected, err := (&Sort{}).renderElm()
		assert.Error(err, "Did not expect to render to Elm: %s", unexpected)
	})

	t.Run("renderGo", func(t *testing.T) {
		unexpected, err := (&Sort{}).renderGo()
		assert.Error(err, "Did not expect to render to Go: %s", unexpected)
	})

	t.Run("renderHaskell", func(t *testing.T) {
		unexpected, err := (&Sort{}).renderHaskell()
		assert.Error(err, "Did not expect to render to Haskell: %s", unexpected)
	})

	t.Run("renderJSON", func(t *testing.T) {
		unexpected, err := (&Sort{}).renderJSON()
		assert.Error(err, "Did not expect to render to JSON: %s", unexpected)
	})

	t.Run("renderJSONSchemaSchema", func(t *testing.T) {
		unexpected, err := (&Sort{}).renderJSONSchema()
		assert.Error(
			err,
			"Did not expect to render to JSONSchema: %s",
			unexpected,
		)
	})

	t.Run("renderJavaScript", func(t *testing.T) {
		unexpected, err := (&Sort{}).renderJavaScript()
		assert.Error(
			err,
			"Did not expect to render to JavaScript: %s",
			unexpected,
		)
	})

	t.Run("renderPureScript", func(t *testing.T) {
		unexpected, err := (&Sort{}).renderPureScript()
		assert.Error(
			err,
			"Did not expect to render to PureScript: %s",
			unexpected,
		)
	})

	t.Run("renderYAML", func(t *testing.T) {
		unexpected, err := (&Sort{}).renderYAML()
		assert.Error(err, "Did not expect to render to YAML: %s", unexpected)
	})

	t.Run("shift", func(t *testing.T) {
		assert.Equal(&Sort{}, (&Sort{}).shift(0, "", 0))
	})

	t.Run("substitute", func(t *testing.T) {
		assert.Equal(
			&Sort{},
			(&Sort{}).substitute("", 0, &BoolValue{Value: true}),
		)
	})
}

func TestType(t *testing.T) {
	assert := require.New(t)

	t.Run("alphaNormalize", func(t *testing.T) {
		assert.Equal(&Type{}, (&Type{}).alphaNormalize())
	})

	t.Run("betaNormalize", func(t *testing.T) {
		assert.Equal(&Type{}, (&Type{}).betaNormalize())
	})

	t.Run("functionCheck", func(t *testing.T) {
		unexpected, err := (&Type{}).functionCheck(&Kind{})
		assert.Error(
			err,
			"Did not expect to allow dependent types: %#v",
			unexpected,
		)
		unexpected, err = (&Type{}).functionCheck(&Sort{})
		assert.Error(
			err,
			"Did not expect to allow dependent types: %#v",
			unexpected,
		)
		actual, err := (&Type{}).functionCheck(&Type{})
		assert.NoError(err)
		assert.Equal(&Type{}, actual)
	})

	t.Run("infer", func(t *testing.T) {
		actual, err := (&Type{}).infer(emptyContext)
		assert.NoError(err)
		assert.Equal(&Kind{}, actual)
	})

	t.Run("render", func(t *testing.T) {
		assert.Equal("Type", pretty.Render(0, (&Type{}).render()))
	})

	t.Run("renderBinary", func(t *testing.T) {
		assert.Equal(binary{value: "Type"}, (&Type{}).renderBinary())
	})

	t.Run("renderCBOR", func(t *testing.T) {
		assert.Equal("\"Type\"", (&Type{}).renderCBOR())
	})

	t.Run("renderElm", func(t *testing.T) {
		unexpected, err := (&Type{}).renderElm()
		assert.Error(err, "Did not expect to render to Elm: %s", unexpected)
	})

	t.Run("renderGo", func(t *testing.T) {
		unexpected, err := (&Type{}).renderGo()
		assert.Error(err, "Did not expect to render to Go: %s", unexpected)
	})

	t.Run("renderHaskell", func(t *testing.T) {
		actual, err := (&Type{}).renderHaskell()
		assert.NoError(err)
		assert.Equal("Type", actual)
	})

	t.Run("renderJSON", func(t *testing.T) {
		unexpected, err := (&Type{}).renderJSON()
		assert.Error(err, "Did not expect to render to JSON: %s", unexpected)
	})

	t.Run("renderJSONSchemaSchema", func(t *testing.T) {
		unexpected, err := (&Type{}).renderJSONSchema()
		assert.Error(
			err,
			"Did not expect to render to JSONSchema: %s",
			unexpected,
		)
	})

	t.Run("renderJavaScript", func(t *testing.T) {
		unexpected, err := (&Type{}).renderJavaScript()
		assert.Error(
			err,
			"Did not expect to render to JavaScript: %s",
			unexpected,
		)
	})

	t.Run("renderPureScript", func(t *testing.T) {
		actual, err := (&Type{}).renderPureScript()
		assert.NoError(err)
		assert.Equal("Type", actual)
	})

	t.Run("renderYAML", func(t *testing.T) {
		unexpected, err := (&Type{}).renderYAML()
		assert.Error(err, "Did not expect to render to YAML: %s", unexpected)
	})

	t.Run("shift", func(t *testing.T) {
		assert.Equal(&Type{}, (&Type{}).shift(0, "", 0))
	})

	t.Run("substitute", func(t *testing.T) {
		assert.Equal(
			&Type{},
			(&Type{}).substitute("", 0, &BoolValue{Value: true}),
		)
	})
}
