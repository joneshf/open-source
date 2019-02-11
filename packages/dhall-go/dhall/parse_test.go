package dhall

import (
	"testing"

	"github.com/sirupsen/logrus"
)

func TestParse(t *testing.T) {
	var log logrus.FieldLogger = &logrus.Logger{}

	t.Run("Bool", func(t *testing.T) {
		actual, err := Parse(&log, []byte("False"))
		expected := &Bool{Value: false}
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual, err = Parse(&log, []byte("True"))
		expected = &Bool{Value: true}
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("BoolEqual", func(t *testing.T) {
		be := &BoolEqual{Left: &Bool{Value: false}, Right: &Bool{Value: false}}
		actual, err := Parse(&log, []byte("False == False"))
		expected := be
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &Bool{Value: false}, Right: &Bool{Value: true}}
		actual, err = Parse(&log, []byte("False == True"))
		expected = be
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &Bool{Value: true}, Right: &Bool{Value: false}}
		actual, err = Parse(&log, []byte("True == False"))
		expected = be
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &Bool{Value: true}, Right: &Bool{Value: true}}
		actual, err = Parse(&log, []byte("True == True"))
		expected = be
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("BoolType", func(t *testing.T) {
		actual, err := Parse(&log, []byte("Bool"))
		expected := &BoolType{}
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("Kind", func(t *testing.T) {
		actual, err := Parse(&log, []byte("Kind"))
		expected := &Kind{}
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("Sort", func(t *testing.T) {
		actual, err := Parse(&log, []byte("Sort"))
		expected := &Sort{}
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("Type", func(t *testing.T) {
		actual, err := Parse(&log, []byte("Type"))
		expected := &Type{}
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})
}
