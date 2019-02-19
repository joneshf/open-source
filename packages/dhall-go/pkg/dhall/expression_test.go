package dhall

import (
	"reflect"
	"testing"

	"github.com/leanovate/gopter"
	"github.com/leanovate/gopter/gen"
	"github.com/leanovate/gopter/prop"
	testifyAssert "github.com/stretchr/testify/assert"
)

func genExpression() gopter.Gen {
	return gen.Weighted([]gen.WeightedGen{
		gen.WeightedGen{Weight: 1, Gen: genBool()},
		gen.WeightedGen{Weight: 5, Gen: genBoolValue()},
	})
}

func TestEquivalent(t *testing.T) {
	assert := testifyAssert.New(t)
	properties := gopter.NewProperties(nil)

	properties.Property("Equivalent is reflexive", prop.ForAll(
		func(e Expression) bool {
			return assert.True(Equivalent(e, e))
		},
		genExpression(),
	))

	properties.Property("Equivalent works correctly", prop.ForAll(
		func(left, right Expression) bool {
			leftNormalized := left.betaNormalize().alphaNormalize()
			rightNormalized := right.betaNormalize().alphaNormalize()
			return !reflect.DeepEqual(leftNormalized, rightNormalized) ||
				assert.True(Equivalent(left, right))
		},
		genExpression(),
		genExpression(),
	))
}
