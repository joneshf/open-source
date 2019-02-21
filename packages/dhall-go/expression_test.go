package dhall

import (
	"reflect"
	"testing"

	"github.com/leanovate/gopter"
	"github.com/leanovate/gopter/gen"
	"github.com/leanovate/gopter/prop"
	testifyAssert "github.com/stretchr/testify/assert"
)

func sized(f func(int) gopter.Gen) gopter.Gen {
	return func(params *gopter.GenParameters) *gopter.GenResult {
		return f(params.Rng.Intn(3))(params)
	}
}

func genExpression() gopter.Gen {
	return sized(genSizedExpression)
}

func genSizedExpression(size int) gopter.Gen {
	if size < 1 {
		return genBoolValue()
	}
	return gen.OneGenOf(
		genAnd(size),
		genEqual(size),
		genIf(size),
		genNotEqual(size),
		genOr(size),
	)
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
