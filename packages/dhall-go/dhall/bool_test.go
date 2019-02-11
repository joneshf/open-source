package dhall

import (
	"reflect"
	"testing"
)

func TestBoolType(t *testing.T) {
	t.Run("alphaNormalize", func(t *testing.T) {
		actual := (&BoolType{}).alphaNormalize()
		expected := &BoolType{}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("betaNormalize", func(t *testing.T) {
		actual := (&BoolType{}).betaNormalize()
		expected := &BoolType{}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("equivalent", func(t *testing.T) {
		left := &BoolType{}
		right := &BoolType{}
		if !Equivalent(left, right) {
			t.Fatalf("Expected %#v to be equivalent to %#v", left, right)
		}
	})

	t.Run("infer", func(t *testing.T) {
		actual, err := (&BoolType{}).infer(Context{value: []annotatedExpression{}})
		expected := &Type{}
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("render", func(t *testing.T) {
		actual := (&BoolType{}).render()
		expected := "Bool"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderBinary", func(t *testing.T) {
		actual := (&BoolType{}).renderBinary()
		expected := binary{value: "Bool"}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderCBOR", func(t *testing.T) {
		actual := (&BoolType{}).renderCBOR()
		expected := "\"Bool\""
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderJSON", func(t *testing.T) {
		unexpected, err := (&BoolType{}).renderJSON()
		if err == nil {
			t.Fatalf("Did not expect to render to JSON: %#v", unexpected)
		}
	})

	t.Run("renderJSONSchema", func(t *testing.T) {
		actual, err := (&BoolType{}).renderJSONSchema()
		expected := "{\"type\": \"boolean\"}"
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderYAML", func(t *testing.T) {
		unexpected, err := (&BoolType{}).renderYAML()
		if err == nil {
			t.Fatalf("Did not expect to render to YAML: %#v", unexpected)
		}
	})

	t.Run("shift", func(t *testing.T) {
		actual := (&BoolType{}).shift(0, "", 0)
		expected := &BoolType{}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("substitute", func(t *testing.T) {
		actual := (&BoolType{}).substitute("", 0, &Bool{Value: true})
		expected := &BoolType{}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})
}

func TestBool(t *testing.T) {
	t.Run("alphaNormalize", func(t *testing.T) {
		actual := (&Bool{Value: false}).alphaNormalize()
		expected := &Bool{Value: false}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual = (&Bool{Value: true}).alphaNormalize()
		expected = &Bool{Value: true}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("betaNormalize", func(t *testing.T) {
		actual := (&Bool{Value: false}).betaNormalize()
		expected := &Bool{Value: false}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual = (&Bool{Value: true}).betaNormalize()
		expected = &Bool{Value: true}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("equivalent", func(t *testing.T) {
		left := &Bool{Value: false}
		right := &Bool{Value: false}
		if !Equivalent(left, right) {
			t.Fatalf("Expected %#v to be equivalent to %#v", left, right)
		}

		right = &Bool{Value: true}
		if Equivalent(left, right) {
			t.Fatalf("Did not expect %#v to be equivalent to %#v", left, right)
		}

		left = &Bool{Value: true}
		right = &Bool{Value: false}
		if Equivalent(left, right) {
			t.Fatalf("Did not expect %#v to be equivalent to %#v", left, right)
		}

		right = &Bool{Value: true}
		if !Equivalent(left, right) {
			t.Fatalf("Expected %#v to be equivalent to %#v", left, right)
		}
	})

	t.Run("infer", func(t *testing.T) {
		actual, err := (&Bool{Value: false}).infer(Context{value: []annotatedExpression{}})
		expected := &BoolType{}
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual, err = (&Bool{Value: true}).infer(Context{value: []annotatedExpression{}})
		expected = &BoolType{}
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("render", func(t *testing.T) {
		actual := (&Bool{Value: false}).render()
		expected := "False"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual = (&Bool{Value: true}).render()
		expected = "True"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderBinary", func(t *testing.T) {
		actual := (&Bool{Value: false}).renderBinary()
		expected := binary{value: false}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual = (&Bool{Value: true}).renderBinary()
		expected = binary{value: true}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderCBOR", func(t *testing.T) {
		actual := (&Bool{Value: false}).renderCBOR()
		expected := "false"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual = (&Bool{Value: true}).renderCBOR()
		expected = "true"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderJSON", func(t *testing.T) {
		actual, err := (&Bool{Value: false}).renderJSON()
		expected := "false"
		if err != nil {
			t.Fatal(err)
		}

		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual, err = (&Bool{Value: true}).renderJSON()
		expected = "true"
		if err != nil {
			t.Fatal(err)
		}

		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderJSONSchema", func(t *testing.T) {
		actual, err := (&Bool{Value: false}).renderJSONSchema()
		expected := "false"
		if err != nil {
			t.Fatal(err)
		}

		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual, err = (&Bool{Value: true}).renderJSONSchema()
		expected = "true"
		if err != nil {
			t.Fatal(err)
		}

		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderYAML", func(t *testing.T) {
		actual, err := (&Bool{Value: false}).renderYAML()
		expected := "false"
		if err != nil {
			t.Fatal(err)
		}

		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual, err = (&Bool{Value: true}).renderYAML()
		expected = "true"
		if err != nil {
			t.Fatal(err)
		}

		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("shift", func(t *testing.T) {
		actual := (&Bool{Value: false}).shift(0, "", 0)
		expected := &Bool{Value: false}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual = (&Bool{Value: true}).shift(0, "", 0)
		expected = &Bool{Value: true}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("substitute", func(t *testing.T) {
		actual := (&Bool{Value: false}).substitute("", 0, &Bool{Value: true})
		expected := &Bool{Value: false}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual = (&Bool{Value: true}).substitute("", 0, &Bool{Value: true})
		expected = &Bool{Value: true}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})
}

func TestBoolEqual(t *testing.T) {
	t.Run("alphaNormalize", func(t *testing.T) {
		be := &BoolEqual{Left: &Bool{Value: false}, Right: &Bool{Value: false}}
		actual := be.alphaNormalize()
		expected := be
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &Bool{Value: false}, Right: &Bool{Value: true}}
		actual = be.alphaNormalize()
		expected = be
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &Bool{Value: true}, Right: &Bool{Value: false}}
		actual = be.alphaNormalize()
		expected = be
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &Bool{Value: true}, Right: &Bool{Value: true}}
		actual = be.alphaNormalize()
		expected = be
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("betaNormalize", func(t *testing.T) {
		be := &BoolEqual{Left: &Bool{Value: false}, Right: &Bool{Value: false}}
		actual := be.betaNormalize()
		expected := &Bool{Value: true}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &Bool{Value: false}, Right: &Bool{Value: true}}
		actual = be.betaNormalize()
		expected = &Bool{Value: false}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &Bool{Value: true}, Right: &Bool{Value: false}}
		actual = be.betaNormalize()
		expected = &Bool{Value: false}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &Bool{Value: true}, Right: &Bool{Value: true}}
		actual = be.betaNormalize()
		expected = &Bool{Value: true}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("equivalent", func(t *testing.T) {
		left := &BoolEqual{Left: &Bool{Value: false}, Right: &Bool{Value: false}}
		right := &Bool{Value: true}
		if !Equivalent(left, right) {
			t.Fatalf("Expected %#v to be equivalent to %#v", left, right)
		}

		left = &BoolEqual{Left: &Bool{Value: false}, Right: &Bool{Value: true}}
		right = &Bool{Value: false}
		if !Equivalent(left, right) {
			t.Fatalf("Expected %#v to be equivalent to %#v", left, right)
		}

		left = &BoolEqual{Left: &Bool{Value: true}, Right: &Bool{Value: false}}
		right = &Bool{Value: false}
		if !Equivalent(left, right) {
			t.Fatalf("Expected %#v to be equivalent to %#v", left, right)
		}

		left = &BoolEqual{Left: &Bool{Value: true}, Right: &Bool{Value: true}}
		right = &Bool{Value: true}
		if !Equivalent(left, right) {
			t.Fatalf("Expected %#v to be equivalent to %#v", left, right)
		}
	})

	t.Run("infer", func(t *testing.T) {
		be := &BoolEqual{Left: &Bool{Value: false}, Right: &Bool{Value: false}}
		actual, err := be.infer(Context{value: []annotatedExpression{}})
		expected := &BoolType{}
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &Bool{Value: false}, Right: &Bool{Value: true}}
		actual, err = be.infer(Context{value: []annotatedExpression{}})
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &Bool{Value: true}, Right: &Bool{Value: false}}
		actual, err = be.infer(Context{value: []annotatedExpression{}})
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &Bool{Value: true}, Right: &Bool{Value: true}}
		actual, err = be.infer(Context{value: []annotatedExpression{}})
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("render", func(t *testing.T) {
		be := &BoolEqual{Left: &Bool{Value: false}, Right: &Bool{Value: false}}
		actual := be.render()
		expected := "False == False"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &Bool{Value: false}, Right: &Bool{Value: true}}
		actual = be.render()
		expected = "False == True"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &Bool{Value: true}, Right: &Bool{Value: false}}
		actual = be.render()
		expected = "True == False"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &Bool{Value: true}, Right: &Bool{Value: true}}
		actual = be.render()
		expected = "True == True"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderBinary", func(t *testing.T) {
		be := &BoolEqual{Left: &Bool{Value: false}, Right: &Bool{Value: false}}
		actual := be.renderBinary()
		expected := binary{value: [](interface{}){3, 2, false, false}}
		if !reflect.DeepEqual(expected.value, actual.value) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected.value, actual.value)
		}

		be = &BoolEqual{Left: &Bool{Value: false}, Right: &Bool{Value: true}}
		actual = be.renderBinary()
		expected = binary{value: [](interface{}){3, 2, false, true}}
		if !reflect.DeepEqual(expected.value, actual.value) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected.value, actual.value)
		}

		be = &BoolEqual{Left: &Bool{Value: true}, Right: &Bool{Value: false}}
		actual = be.renderBinary()
		expected = binary{value: [](interface{}){3, 2, true, false}}
		if !reflect.DeepEqual(expected.value, actual.value) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected.value, actual.value)
		}

		be = &BoolEqual{Left: &Bool{Value: true}, Right: &Bool{Value: true}}
		actual = be.renderBinary()
		expected = binary{value: [](interface{}){3, 2, true, true}}
		if !reflect.DeepEqual(expected.value, actual.value) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected.value, actual.value)
		}
	})

	t.Run("renderCBOR", func(t *testing.T) {
		be := &BoolEqual{Left: &Bool{Value: false}, Right: &Bool{Value: false}}
		actual := be.renderCBOR()
		expected := "[3, 2, false, false]"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &Bool{Value: false}, Right: &Bool{Value: true}}
		actual = be.renderCBOR()
		expected = "[3, 2, false, true]"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &Bool{Value: true}, Right: &Bool{Value: false}}
		actual = be.renderCBOR()
		expected = "[3, 2, true, false]"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &Bool{Value: true}, Right: &Bool{Value: true}}
		actual = be.renderCBOR()
		expected = "[3, 2, true, true]"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderJSON", func(t *testing.T) {
		be := &BoolEqual{Left: &Bool{Value: false}, Right: &Bool{Value: false}}
		unexpected, err := be.renderJSON()
		if err == nil {
			t.Fatalf("Did not expect to render to JSON: %#v", unexpected)
		}
	})

	t.Run("renderJSONSchema", func(t *testing.T) {
		be := &BoolEqual{Left: &Bool{Value: false}, Right: &Bool{Value: false}}
		unexpected, err := be.renderJSONSchema()
		if err == nil {
			t.Fatalf("Did not expect to render to JSONSchema: %#v", unexpected)
		}
	})

	t.Run("renderYAML", func(t *testing.T) {
		be := &BoolEqual{Left: &Bool{Value: false}, Right: &Bool{Value: false}}
		unexpected, err := be.renderYAML()
		if err == nil {
			t.Fatalf("Did not expect to render to YAML: %#v", unexpected)
		}
	})

	t.Run("shift", func(t *testing.T) {
		be := &BoolEqual{Left: &Bool{Value: false}, Right: &Bool{Value: false}}
		actual := be.shift(0, "", 0)
		expected := be
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &Bool{Value: false}, Right: &Bool{Value: true}}
		actual = be.shift(0, "", 0)
		expected = be
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &Bool{Value: true}, Right: &Bool{Value: false}}
		actual = be.shift(0, "", 0)
		expected = be
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &Bool{Value: true}, Right: &Bool{Value: true}}
		actual = be.shift(0, "", 0)
		expected = be
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("substitute", func(t *testing.T) {
		be := &BoolEqual{Left: &Bool{Value: false}, Right: &Bool{Value: false}}
		actual := be.substitute("", 0, &Bool{Value: false})
		expected := be
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &Bool{Value: false}, Right: &Bool{Value: true}}
		actual = be.substitute("", 0, &Bool{Value: false})
		expected = be
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &Bool{Value: true}, Right: &Bool{Value: false}}
		actual = be.substitute("", 0, &Bool{Value: false})
		expected = be
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &Bool{Value: true}, Right: &Bool{Value: true}}
		actual = be.substitute("", 0, &Bool{Value: false})
		expected = be
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})
}
