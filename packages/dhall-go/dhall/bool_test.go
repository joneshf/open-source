package dhall

import (
	"testing"
)

func TestBoolType(t *testing.T) {
	t.Run("alphaNormalize", func(t *testing.T) {
		actual := (&BoolType{}).alphaNormalize()
		expected := &BoolType{}
		if expected != actual {
			t.Errorf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("betaNormalize", func(t *testing.T) {
		actual := (&BoolType{}).betaNormalize()
		expected := &BoolType{}
		if expected != actual {
			t.Errorf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("encode", func(t *testing.T) {
		actual := (&BoolType{}).encode()
		expected := cbor{value: "Bool"}
		if expected != actual {
			t.Errorf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("infer", func(t *testing.T) {
		actual, err := (&BoolType{}).infer(Context{value: []annotatedExpression{}})
		expected := &Type{}
		if err != nil {
			t.Error(err)
		}
		if expected != actual {
			t.Errorf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("shift", func(t *testing.T) {
		actual := (&BoolType{}).shift(0, "", 0)
		expected := &BoolType{}
		if expected != actual {
			t.Errorf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("substitute", func(t *testing.T) {
		actual := (&BoolType{}).substitute("", 0, &Bool{Value: true})
		expected := &BoolType{}
		if expected != actual {
			t.Errorf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})
}

func TestBool(t *testing.T) {
	t.Run("alphaNormalize", func(t *testing.T) {
		actual := (&Bool{Value: false}).alphaNormalize()
		expected := &Bool{Value: false}
		if *expected != *actual.(*Bool) {
			t.Errorf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual = (&Bool{Value: true}).alphaNormalize()
		expected = &Bool{Value: true}
		if *expected != *actual.(*Bool) {
			t.Errorf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("betaNormalize", func(t *testing.T) {
		actual := (&Bool{Value: false}).betaNormalize()
		expected := &Bool{Value: false}
		if *expected != *actual.(*Bool) {
			t.Errorf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual = (&Bool{Value: true}).betaNormalize()
		expected = &Bool{Value: true}
		if *expected != *actual.(*Bool) {
			t.Errorf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("encode", func(t *testing.T) {
		actual := (&Bool{Value: false}).encode()
		expected := cbor{value: false}
		if expected != actual {
			t.Errorf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual = (&Bool{Value: true}).encode()
		expected = cbor{value: true}
		if expected != actual {
			t.Errorf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("infer", func(t *testing.T) {
		actual, err := (&Bool{Value: false}).infer(Context{value: []annotatedExpression{}})
		expected := &BoolType{}
		if err != nil {
			t.Error(err)
		}
		if expected != actual.(*BoolType) {
			t.Errorf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual, err = (&Bool{Value: true}).infer(Context{value: []annotatedExpression{}})
		expected = &BoolType{}
		if err != nil {
			t.Error(err)
		}
		if expected != actual.(*BoolType) {
			t.Errorf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("shift", func(t *testing.T) {
		actual := (&Bool{Value: false}).shift(0, "", 0)
		expected := &Bool{Value: false}
		if *expected != *actual.(*Bool) {
			t.Errorf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual = (&Bool{Value: true}).shift(0, "", 0)
		expected = &Bool{Value: true}
		if *expected != *actual.(*Bool) {
			t.Errorf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("substitute", func(t *testing.T) {
		actual := (&Bool{Value: false}).substitute("", 0, &Bool{Value: true})
		expected := &Bool{Value: false}
		if *expected != *actual.(*Bool) {
			t.Errorf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual = (&Bool{Value: true}).substitute("", 0, &Bool{Value: true})
		expected = &Bool{Value: true}
		if *expected != *actual.(*Bool) {
			t.Errorf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})
}
