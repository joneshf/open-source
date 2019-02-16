package dhall

import (
	"testing"

	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/require"
)

func TestParse(t *testing.T) {
	var log logrus.FieldLogger = &logrus.Logger{}

	assert := require.New(t)

	t.Run("BoolValue", func(t *testing.T) {
		actual, err := Parse(&log, []byte("False"))
		assert.NoError(err)
		assert.Equal(&BoolValue{Value: false}, actual)
		actual, err = Parse(&log, []byte("True"))
		assert.NoError(err)
		assert.Equal(&BoolValue{Value: true}, actual)
	})

	t.Run("Equal", func(t *testing.T) {
		actual, err := Parse(&log, []byte("False == False"))
		assert.NoError(err)
		assert.Equal(
			&Equal{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: false},
			},
			actual,
		)
		actual, err = Parse(&log, []byte("False == True"))
		assert.NoError(err)
		assert.Equal(
			&Equal{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: true},
			},
			actual,
		)
		actual, err = Parse(&log, []byte("True == False"))
		assert.NoError(err)
		assert.Equal(
			&Equal{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: false},
			},
			actual,
		)
		actual, err = Parse(&log, []byte("True == True"))
		assert.NoError(err)
		assert.Equal(
			&Equal{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: true},
			},
			actual,
		)
	})

	t.Run("NotEqual", func(t *testing.T) {
		actual, err := Parse(&log, []byte("False != False"))
		assert.NoError(err)
		assert.Equal(
			&NotEqual{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: false},
			},
			actual,
		)
		actual, err = Parse(&log, []byte("False != True"))
		assert.NoError(err)
		assert.Equal(
			&NotEqual{
				Left:  &BoolValue{Value: false},
				Right: &BoolValue{Value: true},
			},
			actual,
		)
		actual, err = Parse(&log, []byte("True != False"))
		assert.NoError(err)
		assert.Equal(
			&NotEqual{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: false},
			},
			actual,
		)
		actual, err = Parse(&log, []byte("True != True"))
		assert.NoError(err)
		assert.Equal(
			&NotEqual{
				Left:  &BoolValue{Value: true},
				Right: &BoolValue{Value: true},
			},
			actual,
		)
	})

	t.Run("Or", func(t *testing.T) {
		actual, err := Parse(&log, []byte("False || False"))
		assert.NoError(err)
		assert.Equal(
			&Or{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: false}},
			actual,
		)
		actual, err = Parse(&log, []byte("False || True"))
		assert.NoError(err)
		assert.Equal(
			&Or{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: true}},
			actual,
		)
		actual, err = Parse(&log, []byte("True || False"))
		assert.NoError(err)
		assert.Equal(
			&Or{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: false}},
			actual,
		)
		actual, err = Parse(&log, []byte("True || True"))
		assert.NoError(err)
		assert.Equal(
			&Or{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: true}},
			actual,
		)
	})

	t.Run("And", func(t *testing.T) {
		actual, err := Parse(&log, []byte("False && False"))
		assert.NoError(err)
		assert.Equal(
			&And{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: false}},
			actual,
		)
		actual, err = Parse(&log, []byte("False && True"))
		assert.NoError(err)
		assert.Equal(
			&And{Left: &BoolValue{Value: false}, Right: &BoolValue{Value: true}},
			actual,
		)
		actual, err = Parse(&log, []byte("True && False"))
		assert.NoError(err)
		assert.Equal(
			&And{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: false}},
			actual,
		)
		actual, err = Parse(&log, []byte("True && True"))
		assert.NoError(err)
		assert.Equal(
			&And{Left: &BoolValue{Value: true}, Right: &BoolValue{Value: true}},
			actual,
		)
	})

	t.Run("Bool", func(t *testing.T) {
		actual, err := Parse(&log, []byte("Bool"))
		assert.NoError(err)
		assert.Equal(&Bool{}, actual)
	})

	t.Run("Kind", func(t *testing.T) {
		actual, err := Parse(&log, []byte("Kind"))
		assert.NoError(err)
		assert.Equal(&Kind{}, actual)
	})

	t.Run("Sort", func(t *testing.T) {
		actual, err := Parse(&log, []byte("Sort"))
		assert.NoError(err)
		assert.Equal(&Sort{}, actual)
	})

	t.Run("Type", func(t *testing.T) {
		actual, err := Parse(&log, []byte("Type"))
		assert.NoError(err)
		assert.Equal(&Type{}, actual)
	})
}
