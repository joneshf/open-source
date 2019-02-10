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

	t.Run("encode", func(t *testing.T) {
		actual := (&BoolType{}).encode()
		expected := cbor{value: "Bool"}
		if !reflect.DeepEqual(expected, actual) {
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

	t.Run("encode", func(t *testing.T) {
		actual := (&Bool{Value: false}).encode()
		expected := cbor{value: false}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual = (&Bool{Value: true}).encode()
		expected = cbor{value: true}
		if !reflect.DeepEqual(expected, actual) {
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

	t.Run("encode", func(t *testing.T) {
		be := &BoolEqual{Left: &Bool{Value: false}, Right: &Bool{Value: false}}
		actual := be.encode()
		expected := cbor{value: [](interface{}){3, 2, false, false}}
		if !reflect.DeepEqual(expected.value, actual.value) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected.value, actual.value)
		}

		be = &BoolEqual{Left: &Bool{Value: false}, Right: &Bool{Value: true}}
		actual = be.encode()
		expected = cbor{value: [](interface{}){3, 2, false, true}}
		if !reflect.DeepEqual(expected.value, actual.value) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected.value, actual.value)
		}

		be = &BoolEqual{Left: &Bool{Value: true}, Right: &Bool{Value: false}}
		actual = be.encode()
		expected = cbor{value: [](interface{}){3, 2, true, false}}
		if !reflect.DeepEqual(expected.value, actual.value) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected.value, actual.value)
		}

		be = &BoolEqual{Left: &Bool{Value: true}, Right: &Bool{Value: true}}
		actual = be.encode()
		expected = cbor{value: [](interface{}){3, 2, true, true}}
		if !reflect.DeepEqual(expected.value, actual.value) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected.value, actual.value)
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
