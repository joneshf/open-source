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
