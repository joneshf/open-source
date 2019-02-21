package pretty

import (
	"reflect"
	"testing"

	"github.com/leanovate/gopter"
	"github.com/leanovate/gopter/gen"
	"github.com/leanovate/gopter/prop"
	testifyAssert "github.com/stretchr/testify/assert"
)

var genNil = gen.Const(&underlyingNil{}).Map(func(u *underlyingNil) underlying {
	return u
})

var genUnderlying = sized(genUnderlyingSized)

func genLine(size int) gopter.Gen {
	return gen.StructPtr(
		reflect.TypeOf(&underlyingLine{}),
		map[string]gopter.Gen{
			"Underlying": genUnderlyingSized(size / 2),
			"Value":      gen.IntRange(0, 100),
		},
	).Map(func(u *underlyingLine) underlying { return u })
}

func genText(size int) gopter.Gen {
	return gen.StructPtr(
		reflect.TypeOf(&underlyingText{}),
		map[string]gopter.Gen{
			"Underlying": genUnderlyingSized(size / 2),
			"Value":      gen.AnyString(),
		},
	).Map(func(u *underlyingText) underlying { return u })
}

func genUnderlyingSized(size int) gopter.Gen {
	if size < 1 {
		return genNil
	}
	return gen.OneGenOf(genLine(size), genText(size))
}

func TestFits(t *testing.T) {
	assert := testifyAssert.New(t)
	properties := gopter.NewProperties(nil)

	properties.Property("negative values never fit", prop.ForAll(
		func(x int, y underlying) bool {
			return assert.False(y.fits(x))
		},
		gen.IntRange(-100, -1),
		genUnderlying,
	))

	properties.TestingRun(t)
}
