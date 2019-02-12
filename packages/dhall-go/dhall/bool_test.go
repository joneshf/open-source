package dhall

import (
	"reflect"
	"testing"
)

func TestBool(t *testing.T) {
	t.Run("alphaNormalize", func(t *testing.T) {
		actual := (&Bool{}).alphaNormalize()
		expected := &Bool{}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("betaNormalize", func(t *testing.T) {
		actual := (&Bool{}).betaNormalize()
		expected := &Bool{}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("equivalent", func(t *testing.T) {
		left := &Bool{}
		right := &Bool{}
		if !Equivalent(left, right) {
			t.Fatalf("Expected %#v to be equivalent to %#v", left, right)
		}
	})

	t.Run("infer", func(t *testing.T) {
		actual, err := (&Bool{}).infer(Context{value: []annotatedExpression{}})
		expected := &Type{}
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("render", func(t *testing.T) {
		actual := (&Bool{}).render()
		expected := "Bool"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderBinary", func(t *testing.T) {
		actual := (&Bool{}).renderBinary()
		expected := binary{value: "Bool"}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderCBOR", func(t *testing.T) {
		actual := (&Bool{}).renderCBOR()
		expected := "\"Bool\""
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderElm", func(t *testing.T) {
		actual, err := (&Bool{}).renderElm()
		expected := "Bool"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderGo", func(t *testing.T) {
		actual, err := (&Bool{}).renderGo()
		expected := "bool"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderHaskell", func(t *testing.T) {
		actual, err := (&Bool{}).renderHaskell()
		expected := "Bool"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderJSON", func(t *testing.T) {
		unexpected, err := (&Bool{}).renderJSON()
		if err == nil {
			t.Fatalf("Did not expect to render to JSON: %#v", unexpected)
		}
	})

	t.Run("renderJSONSchema", func(t *testing.T) {
		actual, err := (&Bool{}).renderJSONSchema()
		expected := "{\"type\": \"boolean\"}"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderJavaScript", func(t *testing.T) {
		unexpected, err := (&Bool{}).renderJSON()
		if err == nil {
			t.Fatalf("Did not expect to render to JavaScript: %#v", unexpected)
		}
	})

	t.Run("renderPureScript", func(t *testing.T) {
		actual, err := (&Bool{}).renderPureScript()
		expected := "Boolean"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderYAML", func(t *testing.T) {
		unexpected, err := (&Bool{}).renderYAML()
		if err == nil {
			t.Fatalf("Did not expect to render to YAML: %#v", unexpected)
		}
	})

	t.Run("shift", func(t *testing.T) {
		actual := (&Bool{}).shift(0, "", 0)
		expected := &Bool{}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("substitute", func(t *testing.T) {
		actual := (&Bool{}).substitute("", 0, &BoolValue{Value: true})
		expected := &Bool{}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})
}

func TestBoolValue(t *testing.T) {
	t.Run("alphaNormalize", func(t *testing.T) {
		actual := (&BoolValue{Value: false}).alphaNormalize()
		expected := &BoolValue{Value: false}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual = (&BoolValue{Value: true}).alphaNormalize()
		expected = &BoolValue{Value: true}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("betaNormalize", func(t *testing.T) {
		actual := (&BoolValue{Value: false}).betaNormalize()
		expected := &BoolValue{Value: false}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual = (&BoolValue{Value: true}).betaNormalize()
		expected = &BoolValue{Value: true}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("equivalent", func(t *testing.T) {
		left := &BoolValue{Value: false}
		right := &BoolValue{Value: false}
		if !Equivalent(left, right) {
			t.Fatalf("Expected %#v to be equivalent to %#v", left, right)
		}

		right = &BoolValue{Value: true}
		if Equivalent(left, right) {
			t.Fatalf("Did not expect %#v to be equivalent to %#v", left, right)
		}

		left = &BoolValue{Value: true}
		right = &BoolValue{Value: false}
		if Equivalent(left, right) {
			t.Fatalf("Did not expect %#v to be equivalent to %#v", left, right)
		}

		right = &BoolValue{Value: true}
		if !Equivalent(left, right) {
			t.Fatalf("Expected %#v to be equivalent to %#v", left, right)
		}
	})

	t.Run("infer", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).infer(Context{value: []annotatedExpression{}})
		expected := &Bool{}
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual, err = (&BoolValue{Value: true}).infer(Context{value: []annotatedExpression{}})
		expected = &Bool{}
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("render", func(t *testing.T) {
		actual := (&BoolValue{Value: false}).render()
		expected := "False"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual = (&BoolValue{Value: true}).render()
		expected = "True"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderBinary", func(t *testing.T) {
		actual := (&BoolValue{Value: false}).renderBinary()
		expected := binary{value: false}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual = (&BoolValue{Value: true}).renderBinary()
		expected = binary{value: true}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderCBOR", func(t *testing.T) {
		actual := (&BoolValue{Value: false}).renderCBOR()
		expected := "false"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual = (&BoolValue{Value: true}).renderCBOR()
		expected = "true"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderElm", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).renderElm()
		expected := "False"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual, err = (&BoolValue{Value: true}).renderElm()
		expected = "True"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderGo", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).renderGo()
		expected := "false"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual, err = (&BoolValue{Value: true}).renderGo()
		expected = "true"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderHaskell", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).renderHaskell()
		expected := "False"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual, err = (&BoolValue{Value: true}).renderHaskell()
		expected = "True"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderJSON", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).renderJSON()
		expected := "false"
		if err != nil {
			t.Fatal(err)
		}

		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual, err = (&BoolValue{Value: true}).renderJSON()
		expected = "true"
		if err != nil {
			t.Fatal(err)
		}

		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderJSONSchema", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).renderJSONSchema()
		expected := "false"
		if err != nil {
			t.Fatal(err)
		}

		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual, err = (&BoolValue{Value: true}).renderJSONSchema()
		expected = "true"
		if err != nil {
			t.Fatal(err)
		}

		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderJavaScript", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).renderJavaScript()
		expected := "false"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual, err = (&BoolValue{Value: true}).renderJavaScript()
		expected = "true"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderPureScript", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).renderPureScript()
		expected := "false"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual, err = (&BoolValue{Value: true}).renderPureScript()
		expected = "true"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderYAML", func(t *testing.T) {
		actual, err := (&BoolValue{Value: false}).renderYAML()
		expected := "false"
		if err != nil {
			t.Fatal(err)
		}

		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual, err = (&BoolValue{Value: true}).renderYAML()
		expected = "true"
		if err != nil {
			t.Fatal(err)
		}

		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("shift", func(t *testing.T) {
		actual := (&BoolValue{Value: false}).shift(0, "", 0)
		expected := &BoolValue{Value: false}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual = (&BoolValue{Value: true}).shift(0, "", 0)
		expected = &BoolValue{Value: true}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("substitute", func(t *testing.T) {
		actual := (&BoolValue{Value: false}).substitute("", 0, &BoolValue{Value: true})
		expected := &BoolValue{Value: false}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual = (&BoolValue{Value: true}).substitute("", 0, &BoolValue{Value: true})
		expected = &BoolValue{Value: true}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})
}

func TestBoolEqual(t *testing.T) {
	t.Run("alphaNormalize", func(t *testing.T) {
		be := &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: false}}
		actual := be.alphaNormalize()
		expected := be
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: true}}
		actual = be.alphaNormalize()
		expected = be
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: false}}
		actual = be.alphaNormalize()
		expected = be
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: true}}
		actual = be.alphaNormalize()
		expected = be
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("betaNormalize", func(t *testing.T) {
		be := &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: false}}
		actual := be.betaNormalize()
		expected := &BoolValue{Value: true}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: true}}
		actual = be.betaNormalize()
		expected = &BoolValue{Value: false}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: false}}
		actual = be.betaNormalize()
		expected = &BoolValue{Value: false}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: true}}
		actual = be.betaNormalize()
		expected = &BoolValue{Value: true}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("equivalent", func(t *testing.T) {
		left := &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: false}}
		right := &BoolValue{Value: true}
		if !Equivalent(left, right) {
			t.Fatalf("Expected %#v to be equivalent to %#v", left, right)
		}

		left = &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: true}}
		right = &BoolValue{Value: false}
		if !Equivalent(left, right) {
			t.Fatalf("Expected %#v to be equivalent to %#v", left, right)
		}

		left = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: false}}
		right = &BoolValue{Value: false}
		if !Equivalent(left, right) {
			t.Fatalf("Expected %#v to be equivalent to %#v", left, right)
		}

		left = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: true}}
		right = &BoolValue{Value: true}
		if !Equivalent(left, right) {
			t.Fatalf("Expected %#v to be equivalent to %#v", left, right)
		}
	})

	t.Run("infer", func(t *testing.T) {
		be := &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: false}}
		actual, err := be.infer(Context{value: []annotatedExpression{}})
		expected := &Bool{}
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: true}}
		actual, err = be.infer(Context{value: []annotatedExpression{}})
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: false}}
		actual, err = be.infer(Context{value: []annotatedExpression{}})
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: true}}
		actual, err = be.infer(Context{value: []annotatedExpression{}})
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("render", func(t *testing.T) {
		be := &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: false}}
		actual := be.render()
		expected := "False == False"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: true}}
		actual = be.render()
		expected = "False == True"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: false}}
		actual = be.render()
		expected = "True == False"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: true}}
		actual = be.render()
		expected = "True == True"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderBinary", func(t *testing.T) {
		be := &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: false}}
		actual := be.renderBinary()
		expected := binary{value: [](interface{}){3, 2, false, false}}
		if !reflect.DeepEqual(expected.value, actual.value) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected.value, actual.value)
		}

		be = &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: true}}
		actual = be.renderBinary()
		expected = binary{value: [](interface{}){3, 2, false, true}}
		if !reflect.DeepEqual(expected.value, actual.value) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected.value, actual.value)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: false}}
		actual = be.renderBinary()
		expected = binary{value: [](interface{}){3, 2, true, false}}
		if !reflect.DeepEqual(expected.value, actual.value) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected.value, actual.value)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: true}}
		actual = be.renderBinary()
		expected = binary{value: [](interface{}){3, 2, true, true}}
		if !reflect.DeepEqual(expected.value, actual.value) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected.value, actual.value)
		}
	})

	t.Run("renderCBOR", func(t *testing.T) {
		be := &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: false}}
		actual := be.renderCBOR()
		expected := "[3, 2, false, false]"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: true}}
		actual = be.renderCBOR()
		expected = "[3, 2, false, true]"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: false}}
		actual = be.renderCBOR()
		expected = "[3, 2, true, false]"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: true}}
		actual = be.renderCBOR()
		expected = "[3, 2, true, true]"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderElm", func(t *testing.T) {
		be := &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: false}}
		actual, err := be.renderElm()
		expected := "False == False"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: true}}
		actual, err = be.renderElm()
		expected = "False == True"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: false}}
		actual, err = be.renderElm()
		expected = "True == False"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: true}}
		actual, err = be.renderElm()
		expected = "True == True"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderGo", func(t *testing.T) {
		be := &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: false}}
		actual, err := be.renderGo()
		expected := "false == false"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: true}}
		actual, err = be.renderGo()
		expected = "false == true"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: false}}
		actual, err = be.renderGo()
		expected = "true == false"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: true}}
		actual, err = be.renderGo()
		expected = "true == true"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderHaskell", func(t *testing.T) {
		be := &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: false}}
		actual, err := be.renderHaskell()
		expected := "False == False"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: true}}
		actual, err = be.renderHaskell()
		expected = "False == True"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: false}}
		actual, err = be.renderHaskell()
		expected = "True == False"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: true}}
		actual, err = be.renderHaskell()
		expected = "True == True"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderJSON", func(t *testing.T) {
		be := &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: false}}
		unexpected, err := be.renderJSON()
		if err == nil {
			t.Fatalf("Did not expect to render to JSON: %#v", unexpected)
		}
	})

	t.Run("renderJSONSchema", func(t *testing.T) {
		be := &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: false}}
		unexpected, err := be.renderJSONSchema()
		if err == nil {
			t.Fatalf("Did not expect to render to JSONSchema: %#v", unexpected)
		}
	})

	t.Run("renderJavaScript", func(t *testing.T) {
		be := &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: false}}
		actual, err := be.renderJavaScript()
		expected := "false === false"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: true}}
		actual, err = be.renderJavaScript()
		expected = "false === true"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: false}}
		actual, err = be.renderJavaScript()
		expected = "true === false"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: true}}
		actual, err = be.renderJavaScript()
		expected = "true === true"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderPureScript", func(t *testing.T) {
		be := &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: false}}
		actual, err := be.renderPureScript()
		expected := "false == false"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: true}}
		actual, err = be.renderPureScript()
		expected = "false == true"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: false}}
		actual, err = be.renderPureScript()
		expected = "true == false"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: true}}
		actual, err = be.renderPureScript()
		expected = "true == true"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderYAML", func(t *testing.T) {
		be := &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: false}}
		unexpected, err := be.renderYAML()
		if err == nil {
			t.Fatalf("Did not expect to render to YAML: %#v", unexpected)
		}
	})

	t.Run("shift", func(t *testing.T) {
		be := &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: false}}
		actual := be.shift(0, "", 0)
		expected := be
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: true}}
		actual = be.shift(0, "", 0)
		expected = be
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: false}}
		actual = be.shift(0, "", 0)
		expected = be
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: true}}
		actual = be.shift(0, "", 0)
		expected = be
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("substitute", func(t *testing.T) {
		be := &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: false}}
		actual := be.substitute("", 0, &BoolValue{Value: false})
		expected := be
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: true}}
		actual = be.substitute("", 0, &BoolValue{Value: false})
		expected = be
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: false}}
		actual = be.substitute("", 0, &BoolValue{Value: false})
		expected = be
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: true}}
		actual = be.substitute("", 0, &BoolValue{Value: false})
		expected = be
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})
}
