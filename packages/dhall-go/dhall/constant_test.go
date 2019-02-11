package dhall

import (
	"reflect"
	"testing"
)

func TestKind(t *testing.T) {
	t.Run("alphaNormalize", func(t *testing.T) {
		actual := (&Kind{}).alphaNormalize()
		expected := &Kind{}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("betaNormalize", func(t *testing.T) {
		actual := (&Kind{}).betaNormalize()
		expected := &Kind{}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("equivalent", func(t *testing.T) {
		left := &Kind{}
		right := &Kind{}
		if !Equivalent(left, right) {
			t.Fatalf("Expected %#v to be equivalent to %#v", left, right)
		}
	})

	t.Run("functionCheck", func(t *testing.T) {
		var expected Constant
		actual, err := (&Kind{}).functionCheck(&Kind{})
		expected = &Kind{}
		if err != nil {
			t.Fatal(err)
		}
		if expected != actual {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual, err = (&Kind{}).functionCheck(&Sort{})
		expected = &Kind{}
		if err == nil {
			t.Fatalf("Did not expect to allow dependent types: %#v", actual)
		}

		actual, err = (&Kind{}).functionCheck(&Type{})
		expected = &Type{}
		if err != nil {
			t.Fatal(err)
		}
		if expected != actual {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("infer", func(t *testing.T) {
		actual, err := (&Kind{}).infer(Context{value: []annotatedExpression{}})
		expected := &Sort{}
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("render", func(t *testing.T) {
		actual := (&Kind{}).render()
		expected := "Kind"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderBinary", func(t *testing.T) {
		actual := (&Kind{}).renderBinary()
		expected := binary{value: "Kind"}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderCBOR", func(t *testing.T) {
		actual := (&Kind{}).renderCBOR()
		expected := "\"Kind\""
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderJSON", func(t *testing.T) {
		unexpected, err := (&Kind{}).renderJSON()
		if err == nil {
			t.Fatalf("Did not expect to render to JSON: %#v", unexpected)
		}
	})

	t.Run("renderJSONSchema", func(t *testing.T) {
		unexpected, err := (&Kind{}).renderJSONSchema()
		if err == nil {
			t.Fatalf("Did not expect to render to JSONSchema: %#v", unexpected)
		}
	})

	t.Run("renderYAML", func(t *testing.T) {
		unexpected, err := (&Kind{}).renderYAML()
		if err == nil {
			t.Fatalf("Did not expect to render to YAML: %#v", unexpected)
		}
	})

	t.Run("shift", func(t *testing.T) {
		actual := (&Kind{}).shift(0, "", 0)
		expected := &Kind{}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("substitute", func(t *testing.T) {
		actual := (&Kind{}).substitute("", 0, &Bool{Value: true})
		expected := &Kind{}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})
}

func TestSort(t *testing.T) {
	t.Run("alphaNormalize", func(t *testing.T) {
		actual := (&Sort{}).alphaNormalize()
		expected := &Sort{}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("betaNormalize", func(t *testing.T) {
		actual := (&Sort{}).betaNormalize()
		expected := &Sort{}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("equivalent", func(t *testing.T) {
		left := &Sort{}
		right := &Sort{}
		if !Equivalent(left, right) {
			t.Fatalf("Expected %#v to be equivalent to %#v", left, right)
		}
	})

	t.Run("functionCheck", func(t *testing.T) {
		var expected Constant
		actual, err := (&Sort{}).functionCheck(&Kind{})
		expected = &Kind{}
		if err != nil {
			t.Fatal(err)
		}
		if expected != actual {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual, err = (&Sort{}).functionCheck(&Sort{})
		expected = &Sort{}
		if err != nil {
			t.Fatal(err)
		}
		if expected != actual {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual, err = (&Sort{}).functionCheck(&Type{})
		expected = &Type{}
		if err != nil {
			t.Fatal(err)
		}
		if expected != actual {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("infer", func(t *testing.T) {
		actual, err := (&Sort{}).infer(Context{value: []annotatedExpression{}})
		if err == nil {
			t.Fatalf("Did not expect to infer type: %#v", actual)
		}
	})

	t.Run("render", func(t *testing.T) {
		actual := (&Sort{}).render()
		expected := "Sort"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderBinary", func(t *testing.T) {
		actual := (&Sort{}).renderBinary()
		expected := binary{value: "Sort"}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderCBOR", func(t *testing.T) {
		actual := (&Sort{}).renderCBOR()
		expected := "\"Sort\""
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderJSON", func(t *testing.T) {
		unexpected, err := (&Sort{}).renderJSON()
		if err == nil {
			t.Fatalf("Did not expect to render to JSON: %#v", unexpected)
		}
	})

	t.Run("renderJSONSchema", func(t *testing.T) {
		unexpected, err := (&Sort{}).renderJSONSchema()
		if err == nil {
			t.Fatalf("Did not expect to render to JSONSchema: %#v", unexpected)
		}
	})

	t.Run("renderYAML", func(t *testing.T) {
		unexpected, err := (&Sort{}).renderYAML()
		if err == nil {
			t.Fatalf("Did not expect to render to YAML: %#v", unexpected)
		}
	})

	t.Run("shift", func(t *testing.T) {
		actual := (&Sort{}).shift(0, "", 0)
		expected := &Sort{}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("substitute", func(t *testing.T) {
		actual := (&Sort{}).substitute("", 0, &Bool{Value: true})
		expected := &Sort{}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})
}

func TestType(t *testing.T) {
	t.Run("alphaNormalize", func(t *testing.T) {
		actual := (&Type{}).alphaNormalize()
		expected := &Type{}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("betaNormalize", func(t *testing.T) {
		actual := (&Type{}).betaNormalize()
		expected := &Type{}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("equivalent", func(t *testing.T) {
		left := &Type{}
		right := &Type{}
		if !Equivalent(left, right) {
			t.Fatalf("Expected %#v to be equivalent to %#v", left, right)
		}
	})

	t.Run("functionCheck", func(t *testing.T) {
		var expected Constant
		actual, err := (&Type{}).functionCheck(&Kind{})
		expected = &Type{}
		if err == nil {
			t.Fatalf("Did not expect to allow dependent types: %#v", actual)
		}

		actual, err = (&Type{}).functionCheck(&Sort{})
		expected = &Type{}
		if err == nil {
			t.Fatalf("Did not expect to allow dependent types: %#v", actual)
		}

		actual, err = (&Type{}).functionCheck(&Type{})
		expected = &Type{}
		if err != nil {
			t.Fatal(err)
		}
		if expected != actual {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("infer", func(t *testing.T) {
		actual, err := (&Type{}).infer(Context{value: []annotatedExpression{}})
		expected := &Kind{}
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("render", func(t *testing.T) {
		actual := (&Type{}).render()
		expected := "Type"
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderBinary", func(t *testing.T) {
		actual := (&Type{}).renderBinary()
		expected := binary{value: "Type"}
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderCBOR", func(t *testing.T) {
		actual := (&Type{}).renderCBOR()
		expected := "\"Type\""
		if !reflect.DeepEqual(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("renderJSON", func(t *testing.T) {
		unexpected, err := (&Type{}).renderJSON()
		if err == nil {
			t.Fatalf("Did not expect to render to JSON: %#v", unexpected)
		}
	})

	t.Run("renderJSONSchema", func(t *testing.T) {
		unexpected, err := (&Type{}).renderJSONSchema()
		if err == nil {
			t.Fatalf("Did not expect to render to JSONSchema: %#v", unexpected)
		}
	})

	t.Run("renderYAML", func(t *testing.T) {
		unexpected, err := (&Type{}).renderYAML()
		if err == nil {
			t.Fatalf("Did not expect to render to YAML: %#v", unexpected)
		}
	})

	t.Run("shift", func(t *testing.T) {
		actual := (&Type{}).shift(0, "", 0)
		expected := &Type{}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("substitute", func(t *testing.T) {
		actual := (&Type{}).substitute("", 0, &Bool{Value: true})
		expected := &Type{}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})
}
