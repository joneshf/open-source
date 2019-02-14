package dhall

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestBool(t *testing.T) {
	assert := require.New(t)

	t.Run("alphaNormalize", func(t *testing.T) {
		assert.Equal(&Bool{}, (&Bool{}).alphaNormalize())
	})

	t.Run("betaNormalize", func(t *testing.T) {
		assert.Equal(&Bool{}, (&Bool{}).betaNormalize())
	})

	t.Run("equivalent", func(t *testing.T) {
		assert.True(Equivalent(&Bool{}, &Bool{}))
	})

	t.Run("infer", func(t *testing.T) {
		actual, err := (&Bool{}).infer(emptyContext)
		assert.NoError(err)
		assert.Equal(&Type{}, actual)
	})

	t.Run("render", func(t *testing.T) {
		assert.Equal("Bool", (&Bool{}).render())
	})

	t.Run("renderBinary", func(t *testing.T) {
		assert.Equal(binary{value: "Bool"}, (&Bool{}).renderBinary())
	})

	t.Run("renderCBOR", func(t *testing.T) {
		assert.Equal("\"Bool\"", (&Bool{}).renderCBOR())
	})

	t.Run("renderElm", func(t *testing.T) {
		actual, err := (&Bool{}).renderElm()
		assert.NoError(err)
		assert.Equal("Bool", actual)
	})

	t.Run("renderGo", func(t *testing.T) {
		actual, err := (&Bool{}).renderGo()
		assert.NoError(err)
		assert.Equal("bool", actual)
	})

	t.Run("renderHaskell", func(t *testing.T) {
		actual, err := (&Bool{}).renderHaskell()
		assert.NoError(err)
		assert.Equal("Bool", actual)
	})

	t.Run("renderJSON", func(t *testing.T) {
		unexpected, err := (&Bool{}).renderJSON()
		assert.Error(err, "Did not expect to render to JSON: %s", unexpected)
	})

	t.Run("renderJSONSchema", func(t *testing.T) {
		actual, err := (&Bool{}).renderJSONSchema()
		assert.NoError(err)
		assert.Equal("{\"type\": \"boolean\"}", actual)
	})

	t.Run("renderJavaScript", func(t *testing.T) {
		unexpected, err := (&Bool{}).renderJavaScript()
		assert.Error(
			err,
			"Did not expect to render to JavaScript: %s",
			unexpected,
		)
	})

	t.Run("renderPureScript", func(t *testing.T) {
		actual, err := (&Bool{}).renderPureScript()
		assert.NoError(err)
		assert.Equal("Boolean", actual)
	})

	t.Run("renderYAML", func(t *testing.T) {
		unexpected, err := (&Bool{}).renderYAML()
		assert.Error(err, "Did not expect to render to YAML: %s", unexpected)
	})

	t.Run("shift", func(t *testing.T) {
		assert.Equal(&Bool{}, (&Bool{}).shift(0, "", 0))
	})

	t.Run("substitute", func(t *testing.T) {
		assert.Equal(
			&Bool{},
			(&Bool{}).substitute("", 0, &BoolValue{Value: true}),
		)
	})
}

func TestBoolValue(t *testing.T) {
	assert := require.New(t)
	t.Run("alphaNormalize", func(t *testing.T) {
		assert.Equal(
			&BoolValue{Value: false},
			(&BoolValue{Value: false}).alphaNormalize(),
		)

		assert.Equal(
			&BoolValue{Value: true},
			(&BoolValue{Value: true}).alphaNormalize(),
		)
	})

	t.Run("betaNormalize", func(t *testing.T) {
		assert.Equal(
			&BoolValue{Value: false},
			(&BoolValue{Value: false}).betaNormalize(),
		)

		assert.Equal(
			&BoolValue{Value: true},
			(&BoolValue{Value: true}).betaNormalize(),
		)
	})

	t.Run("equivalent", func(t *testing.T) {
		assert.Equal(&BoolValue{Value: false}, (&BoolValue{Value: false}))
		assert.NotEqual(&BoolValue{Value: false}, (&BoolValue{Value: true}))
		assert.NotEqual(&BoolValue{Value: true}, (&BoolValue{Value: false}))
		assert.Equal(&BoolValue{Value: true}, (&BoolValue{Value: true}))
	})

	t.Run("infer", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).infer(emptyContext)
		assert.NoError(err)
		assert.Equal(&Bool{}, actual)
		actual, err = (&BoolValue{Value: true}).infer(emptyContext)
		assert.NoError(err)
		assert.Equal(&Bool{}, actual)
	})

	t.Run("render", func(t *testing.T) {
		assert.Equal("False", (&BoolValue{Value: false}).render())
		assert.Equal("True", (&BoolValue{Value: true}).render())
	})

	t.Run("renderBinary", func(t *testing.T) {
		assert.Equal(
			binary{value: false},
			(&BoolValue{Value: false}).renderBinary(),
		)
		assert.Equal(
			binary{value: true},
			(&BoolValue{Value: true}).renderBinary(),
		)
	})

	t.Run("renderCBOR", func(t *testing.T) {
		assert.Equal("false", (&BoolValue{Value: false}).renderCBOR())
		assert.Equal("true", (&BoolValue{Value: true}).renderCBOR())
	})

	t.Run("renderElm", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).renderElm()
		assert.NoError(err)
		assert.Equal("False", actual)
		actual, err = (&BoolValue{Value: true}).renderElm()
		assert.NoError(err)
		assert.Equal("True", actual)
	})

	t.Run("renderGo", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).renderGo()
		assert.NoError(err)
		assert.Equal("false", actual)
		actual, err = (&BoolValue{Value: true}).renderGo()
		assert.NoError(err)
		assert.Equal("true", actual)
	})

	t.Run("renderHaskell", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).renderHaskell()
		assert.NoError(err)
		assert.Equal("False", actual)
		actual, err = (&BoolValue{Value: true}).renderHaskell()
		assert.NoError(err)
		assert.Equal("True", actual)
	})

	t.Run("renderJSON", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).renderJSON()
		assert.NoError(err)
		assert.Equal("false", actual)
		actual, err = (&BoolValue{Value: true}).renderJSON()
		assert.NoError(err)
		assert.Equal("true", actual)
	})

	t.Run("renderJSONSchema", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).renderJSONSchema()
		assert.NoError(err)
		assert.Equal("false", actual)
		actual, err = (&BoolValue{Value: true}).renderJSONSchema()
		assert.NoError(err)
		assert.Equal("true", actual)
	})

	t.Run("renderJavaScript", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).renderJavaScript()
		assert.NoError(err)
		assert.Equal("false", actual)
		actual, err = (&BoolValue{Value: true}).renderJavaScript()
		assert.NoError(err)
		assert.Equal("true", actual)
	})

	t.Run("renderPureScript", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).renderPureScript()
		assert.NoError(err)
		assert.Equal("false", actual)
		actual, err = (&BoolValue{Value: true}).renderPureScript()
		assert.NoError(err)
		assert.Equal("true", actual)
	})

	t.Run("renderYAML", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).renderYAML()
		assert.NoError(err)
		assert.Equal("false", actual)
		actual, err = (&BoolValue{Value: true}).renderYAML()
		assert.NoError(err)
		assert.Equal("true", actual)
	})

	t.Run("shift", func(t *testing.T) {
		assert.Equal(
			&BoolValue{Value: false},
			(&BoolValue{Value: false}).shift(0, "", 0),
		)
		assert.Equal(
			&BoolValue{Value: true},
			(&BoolValue{Value: true}).shift(0, "", 0),
		)
	})

	t.Run("substitute", func(t *testing.T) {
		assert.Equal(
			&BoolValue{Value: false},
			(&BoolValue{Value: false}).substitute("", 0, &BoolValue{Value: true}),
		)
		assert.Equal(
			&BoolValue{Value: true},
			(&BoolValue{Value: true}).substitute("", 0, &BoolValue{Value: true}),
		)
	})
}

func TestBoolEqual(t *testing.T) {
	assert := require.New(t)

	t.Run("alphaNormalize", func(t *testing.T) {
		be := &BoolEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}
		assert.Equal(be, be.alphaNormalize())
		be = &BoolEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: true},
		}
		assert.Equal(be, be.alphaNormalize())
		be = &BoolEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: false},
		}
		assert.Equal(be, be.alphaNormalize())
		be = &BoolEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: true},
		}
		assert.Equal(be, be.alphaNormalize())
	})

	t.Run("betaNormalize", func(t *testing.T) {
		be := &BoolEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}
		assert.Equal(&BoolValue{Value: true}, be.betaNormalize())
		be = &BoolEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: true},
		}
		assert.Equal(&BoolValue{Value: false}, be.betaNormalize())
		be = &BoolEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: false},
		}
		assert.Equal(&BoolValue{Value: false}, be.betaNormalize())
		be = &BoolEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: true},
		}
		assert.Equal(&BoolValue{Value: true}, be.betaNormalize())
	})

	t.Run("equivalent", func(t *testing.T) {
		assert.True(Equivalent(
			&BoolValue{Value: true},
			&BoolEqual{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: false},
			},
		))
		assert.True(Equivalent(
			&BoolValue{Value: false},
			&BoolEqual{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: true},
			},
		))
		assert.True(Equivalent(
			&BoolValue{Value: false},
			&BoolEqual{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: false},
			},
		))
		assert.True(Equivalent(
			&BoolValue{Value: true},
			&BoolEqual{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: true},
			},
		))
	})

	t.Run("infer", func(t *testing.T) {
		actual, err := (&BoolEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}).infer(emptyContext)
		assert.NoError(err)
		assert.Equal(&Bool{}, actual)
		actual, err = (&BoolEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: true},
		}).infer(emptyContext)
		assert.NoError(err)
		assert.Equal(&Bool{}, actual)
		actual, err = (&BoolEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: false},
		}).infer(emptyContext)
		assert.NoError(err)
		assert.Equal(&Bool{}, actual)
		actual, err = (&BoolEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: true},
		}).infer(emptyContext)
		assert.NoError(err)
		assert.Equal(&Bool{}, actual)
	})

	t.Run("render", func(t *testing.T) {
		assert.Equal(
			"False == False",
			(&BoolEqual{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: false},
			}).render(),
		)
		assert.Equal(
			"False == True",
			(&BoolEqual{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: true},
			}).render(),
		)
		assert.Equal(
			"True == False",
			(&BoolEqual{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: false},
			}).render(),
		)
		assert.Equal(
			"True == True",
			(&BoolEqual{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: true},
			}).render(),
		)
	})

	t.Run("renderBinary", func(t *testing.T) {
		assert.Equal(
			binary{value: [](interface{}){3, 2, false, false}},
			(&BoolEqual{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: false},
			}).renderBinary(),
		)
		assert.Equal(
			binary{value: [](interface{}){3, 2, false, true}},
			(&BoolEqual{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: true},
			}).renderBinary(),
		)
		assert.Equal(
			binary{value: [](interface{}){3, 2, true, false}},
			(&BoolEqual{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: false},
			}).renderBinary(),
		)
		assert.Equal(
			binary{value: [](interface{}){3, 2, true, true}},
			(&BoolEqual{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: true},
			}).renderBinary(),
		)
	})

	t.Run("renderCBOR", func(t *testing.T) {
		assert.Equal(
			"[3, 2, false, false]",
			(&BoolEqual{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: false},
			}).renderCBOR(),
		)
		assert.Equal(
			"[3, 2, false, true]",
			(&BoolEqual{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: true},
			}).renderCBOR(),
		)
		assert.Equal(
			"[3, 2, true, false]",
			(&BoolEqual{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: false},
			}).renderCBOR(),
		)
		assert.Equal(
			"[3, 2, true, true]",
			(&BoolEqual{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: true},
			}).renderCBOR(),
		)
	})

	t.Run("renderElm", func(t *testing.T) {
		actual, err := (&BoolEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}).renderElm()
		assert.NoError(err)
		assert.Equal("False == False", actual)
		actual, err = (&BoolEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: true},
		}).renderElm()
		assert.NoError(err)
		assert.Equal("False == True", actual)
		actual, err = (&BoolEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: false},
		}).renderElm()
		assert.NoError(err)
		assert.Equal("True == False", actual)
		actual, err = (&BoolEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: true},
		}).renderElm()
		assert.NoError(err)
		assert.Equal("True == True", actual)
	})

	t.Run("renderGo", func(t *testing.T) {
		actual, err := (&BoolEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}).renderGo()
		assert.NoError(err)
		assert.Equal("false == false", actual)
		actual, err = (&BoolEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: true},
		}).renderGo()
		assert.NoError(err)
		assert.Equal("false == true", actual)
		actual, err = (&BoolEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: false},
		}).renderGo()
		assert.NoError(err)
		assert.Equal("true == false", actual)
		actual, err = (&BoolEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: true},
		}).renderGo()
		assert.NoError(err)
		assert.Equal("true == true", actual)
	})

	t.Run("renderHaskell", func(t *testing.T) {
		actual, err := (&BoolEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}).renderHaskell()
		assert.NoError(err)
		assert.Equal("False == False", actual)
		actual, err = (&BoolEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: true},
		}).renderHaskell()
		assert.NoError(err)
		assert.Equal("False == True", actual)
		actual, err = (&BoolEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: false},
		}).renderHaskell()
		assert.NoError(err)
		assert.Equal("True == False", actual)
		actual, err = (&BoolEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: true},
		}).renderHaskell()
		assert.NoError(err)
		assert.Equal("True == True", actual)
	})

	t.Run("renderJSON", func(t *testing.T) {
		unexpected, err := (&BoolEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}).renderJSON()
		assert.Error(err, "Did not expect to render to JSON: %s", unexpected)
	})

	t.Run("renderJSONSchema", func(t *testing.T) {
		unexpected, err := (&BoolEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}).renderJSONSchema()
		assert.Error(
			err,
			"Did not expect to render to JSONSchema: %s",
			unexpected,
		)
	})

	t.Run("renderJavaScript", func(t *testing.T) {
		actual, err := (&BoolEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}).renderJavaScript()
		assert.NoError(err)
		assert.Equal("false === false", actual)
		actual, err = (&BoolEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: true},
		}).renderJavaScript()
		assert.NoError(err)
		assert.Equal("false === true", actual)
		actual, err = (&BoolEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: false},
		}).renderJavaScript()
		assert.NoError(err)
		assert.Equal("true === false", actual)
		actual, err = (&BoolEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: true},
		}).renderJavaScript()
		assert.NoError(err)
		assert.Equal("true === true", actual)
	})

	t.Run("renderPureScript", func(t *testing.T) {
		actual, err := (&BoolEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}).renderPureScript()
		assert.NoError(err)
		assert.Equal("false == false", actual)
		actual, err = (&BoolEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: true},
		}).renderPureScript()
		assert.NoError(err)
		assert.Equal("false == true", actual)
		actual, err = (&BoolEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: false},
		}).renderPureScript()
		assert.NoError(err)
		assert.Equal("true == false", actual)
		actual, err = (&BoolEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: true},
		}).renderPureScript()
		assert.NoError(err)
		assert.Equal("true == true", actual)
	})

	t.Run("renderYAML", func(t *testing.T) {
		unexpected, err := (&BoolEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}).renderYAML()
		assert.Error(err, "Did not expect to render to YAML: %s", unexpected)
	})

	t.Run("shift", func(t *testing.T) {
		be := &BoolEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}
		assert.Equal(be, be.shift(0, "", 0))
		be = &BoolEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: true},
		}
		assert.Equal(be, be.shift(0, "", 0))
		be = &BoolEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: false},
		}
		assert.Equal(be, be.shift(0, "", 0))
		be = &BoolEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: true},
		}
		assert.Equal(be, be.shift(0, "", 0))
	})

	t.Run("substitute", func(t *testing.T) {
		be := &BoolEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}
		assert.Equal(be, be.substitute("", 0, &BoolValue{Value: false}))
		be = &BoolEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: true},
		}
		assert.Equal(be, be.substitute("", 0, &BoolValue{Value: false}))
		be = &BoolEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: false},
		}
		assert.Equal(be, be.substitute("", 0, &BoolValue{Value: false}))
		be = &BoolEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: true},
		}
		assert.Equal(be, be.substitute("", 0, &BoolValue{Value: false}))
	})
}

func TestBoolNotEqual(t *testing.T) {
	assert := require.New(t)

	t.Run("alphaNormalize", func(t *testing.T) {
		be := &BoolNotEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}
		assert.Equal(be, be.alphaNormalize())
		be = &BoolNotEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: true},
		}
		assert.Equal(be, be.alphaNormalize())
		be = &BoolNotEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: false},
		}
		assert.Equal(be, be.alphaNormalize())
		be = &BoolNotEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: true},
		}
		assert.Equal(be, be.alphaNormalize())
	})

	t.Run("betaNormalize", func(t *testing.T) {
		be := &BoolNotEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}
		assert.Equal(&BoolValue{Value: false}, be.betaNormalize())
		be = &BoolNotEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: true},
		}
		assert.Equal(&BoolValue{Value: true}, be.betaNormalize())
		be = &BoolNotEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: false},
		}
		assert.Equal(&BoolValue{Value: true}, be.betaNormalize())
		be = &BoolNotEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: true},
		}
		assert.Equal(&BoolValue{Value: false}, be.betaNormalize())
	})

	t.Run("equivalent", func(t *testing.T) {
		assert.True(Equivalent(
			&BoolValue{Value: false},
			&BoolNotEqual{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: false},
			},
		))
		assert.True(Equivalent(
			&BoolValue{Value: true},
			&BoolNotEqual{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: true},
			},
		))
		assert.True(Equivalent(
			&BoolValue{Value: true},
			&BoolNotEqual{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: false},
			},
		))
		assert.True(Equivalent(
			&BoolValue{Value: false},
			&BoolNotEqual{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: true},
			},
		))
	})

	t.Run("infer", func(t *testing.T) {
		actual, err := (&BoolNotEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}).infer(emptyContext)
		assert.NoError(err)
		assert.Equal(&Bool{}, actual)
		actual, err = (&BoolNotEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: true},
		}).infer(emptyContext)
		assert.NoError(err)
		assert.Equal(&Bool{}, actual)
		actual, err = (&BoolNotEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: false},
		}).infer(emptyContext)
		assert.NoError(err)
		assert.Equal(&Bool{}, actual)
		actual, err = (&BoolNotEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: true},
		}).infer(emptyContext)
		assert.NoError(err)
		assert.Equal(&Bool{}, actual)
	})

	t.Run("render", func(t *testing.T) {
		assert.Equal(
			"False != False",
			(&BoolNotEqual{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: false},
			}).render(),
		)
		assert.Equal(
			"False != True",
			(&BoolNotEqual{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: true},
			}).render(),
		)
		assert.Equal(
			"True != False",
			(&BoolNotEqual{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: false},
			}).render(),
		)
		assert.Equal(
			"True != True",
			(&BoolNotEqual{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: true},
			}).render(),
		)
	})

	t.Run("renderBinary", func(t *testing.T) {
		assert.Equal(
			binary{value: [](interface{}){3, 3, false, false}},
			(&BoolNotEqual{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: false},
			}).renderBinary(),
		)
		assert.Equal(
			binary{value: [](interface{}){3, 3, false, true}},
			(&BoolNotEqual{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: true},
			}).renderBinary(),
		)
		assert.Equal(
			binary{value: [](interface{}){3, 3, true, false}},
			(&BoolNotEqual{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: false},
			}).renderBinary(),
		)
		assert.Equal(
			binary{value: [](interface{}){3, 3, true, true}},
			(&BoolNotEqual{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: true},
			}).renderBinary(),
		)
	})

	t.Run("renderCBOR", func(t *testing.T) {
		assert.Equal(
			"[3, 3, false, false]",
			(&BoolNotEqual{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: false},
			}).renderCBOR(),
		)
		assert.Equal(
			"[3, 3, false, true]",
			(&BoolNotEqual{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: true},
			}).renderCBOR(),
		)
		assert.Equal(
			"[3, 3, true, false]",
			(&BoolNotEqual{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: false},
			}).renderCBOR(),
		)
		assert.Equal(
			"[3, 3, true, true]",
			(&BoolNotEqual{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: true},
			}).renderCBOR(),
		)
	})

	t.Run("renderElm", func(t *testing.T) {
		actual, err := (&BoolNotEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}).renderElm()
		assert.NoError(err)
		assert.Equal("False /= False", actual)
		actual, err = (&BoolNotEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: true},
		}).renderElm()
		assert.NoError(err)
		assert.Equal("False /= True", actual)
		actual, err = (&BoolNotEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: false},
		}).renderElm()
		assert.NoError(err)
		assert.Equal("True /= False", actual)
		actual, err = (&BoolNotEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: true},
		}).renderElm()
		assert.NoError(err)
		assert.Equal("True /= True", actual)
	})

	t.Run("renderGo", func(t *testing.T) {
		actual, err := (&BoolNotEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}).renderGo()
		assert.NoError(err)
		assert.Equal("false != false", actual)
		actual, err = (&BoolNotEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: true},
		}).renderGo()
		assert.NoError(err)
		assert.Equal("false != true", actual)
		actual, err = (&BoolNotEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: false},
		}).renderGo()
		assert.NoError(err)
		assert.Equal("true != false", actual)
		actual, err = (&BoolNotEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: true},
		}).renderGo()
		assert.NoError(err)
		assert.Equal("true != true", actual)
	})

	t.Run("renderHaskell", func(t *testing.T) {
		actual, err := (&BoolNotEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}).renderHaskell()
		assert.NoError(err)
		assert.Equal("False /= False", actual)
		actual, err = (&BoolNotEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: true},
		}).renderHaskell()
		assert.NoError(err)
		assert.Equal("False /= True", actual)
		actual, err = (&BoolNotEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: false},
		}).renderHaskell()
		assert.NoError(err)
		assert.Equal("True /= False", actual)
		actual, err = (&BoolNotEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: true},
		}).renderHaskell()
		assert.NoError(err)
		assert.Equal("True /= True", actual)
	})

	t.Run("renderJSON", func(t *testing.T) {
		unexpected, err := (&BoolNotEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}).renderJSON()
		assert.Error(err, "Did not expect to render to JSON: %s", unexpected)
	})

	t.Run("renderJSONSchema", func(t *testing.T) {
		unexpected, err := (&BoolNotEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}).renderJSONSchema()
		assert.Error(
			err,
			"Did not expect to render to JSONSchema: %s",
			unexpected,
		)
	})

	t.Run("renderJavaScript", func(t *testing.T) {
		actual, err := (&BoolNotEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}).renderJavaScript()
		assert.NoError(err)
		assert.Equal("false !== false", actual)
		actual, err = (&BoolNotEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: true},
		}).renderJavaScript()
		assert.NoError(err)
		assert.Equal("false !== true", actual)
		actual, err = (&BoolNotEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: false},
		}).renderJavaScript()
		assert.NoError(err)
		assert.Equal("true !== false", actual)
		actual, err = (&BoolNotEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: true},
		}).renderJavaScript()
		assert.NoError(err)
		assert.Equal("true !== true", actual)
	})

	t.Run("renderPureScript", func(t *testing.T) {
		actual, err := (&BoolNotEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}).renderPureScript()
		assert.NoError(err)
		assert.Equal("false /= false", actual)
		actual, err = (&BoolNotEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: true},
		}).renderPureScript()
		assert.NoError(err)
		assert.Equal("false /= true", actual)
		actual, err = (&BoolNotEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: false},
		}).renderPureScript()
		assert.NoError(err)
		assert.Equal("true /= false", actual)
		actual, err = (&BoolNotEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: true},
		}).renderPureScript()
		assert.NoError(err)
		assert.Equal("true /= true", actual)
	})

	t.Run("renderYAML", func(t *testing.T) {
		unexpected, err := (&BoolNotEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}).renderYAML()
		assert.Error(err, "Did not expect to render to YAML: %s", unexpected)
	})

	t.Run("shift", func(t *testing.T) {
		be := &BoolNotEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}
		assert.Equal(be, be.shift(0, "", 0))
		be = &BoolNotEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: true},
		}
		assert.Equal(be, be.shift(0, "", 0))
		be = &BoolNotEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: false},
		}
		assert.Equal(be, be.shift(0, "", 0))
		be = &BoolNotEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: true},
		}
		assert.Equal(be, be.shift(0, "", 0))
	})

	t.Run("substitute", func(t *testing.T) {
		be := &BoolNotEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: false},
		}
		assert.Equal(be, be.substitute("", 0, &BoolValue{Value: false}))
		be = &BoolNotEqual{
			Left:  &BoolValue{Value: false},
			Right: &BoolValue{Value: true},
		}
		assert.Equal(be, be.substitute("", 0, &BoolValue{Value: false}))
		be = &BoolNotEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: false},
		}
		assert.Equal(be, be.substitute("", 0, &BoolValue{Value: false}))
		be = &BoolNotEqual{
			Left:  &BoolValue{Value: true},
			Right: &BoolValue{Value: true},
		}
		assert.Equal(be, be.substitute("", 0, &BoolValue{Value: false}))
	})
}
