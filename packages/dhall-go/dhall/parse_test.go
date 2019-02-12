package dhall

import (
	"testing"

	"github.com/sirupsen/logrus"
)

func TestParse(t *testing.T) {
	var log logrus.FieldLogger = &logrus.Logger{}

	t.Run("BoolValue", func(t *testing.T) {
		actual, err := Parse(&log, []byte("False"))
		expected := &BoolValue{Value: false}
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		actual, err = Parse(&log, []byte("True"))
		expected = &BoolValue{Value: true}
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("BoolEqual", func(t *testing.T) {
		be := &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: false}}
		actual, err := Parse(&log, []byte("False == False"))
		expected := be
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: true}}
		actual, err = Parse(&log, []byte("False == True"))
		expected = be
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: false}}
		actual, err = Parse(&log, []byte("True == False"))
		expected = be
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}

		be = &BoolEqual{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: true}}
		actual, err = Parse(&log, []byte("True == True"))
		expected = be
		if err != nil {
			t.Fatal(err)
		}
		if !Equivalent(expected, actual) {
			t.Fatalf("Expected: %#v, Actual: %#v", expected, actual)
		}
	})

	t.Run("Bool", func(t *testing.T) {
		actual, err := Parse(&log, []byte("Bool"))
		expected := &Bool{}
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
